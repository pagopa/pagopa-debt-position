
package it.gov.pagopa.payments.service;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import it.gov.pagopa.payments.enumeration.PaymentOptionStatusEnum;
import it.gov.pagopa.payments.repository.PaymentOptionsRepository;
import it.gov.pagopa.payments.enumeration.JobStatusEnum;
import it.gov.pagopa.payments.enumeration.PaymentStatusEnum;
import it.gov.pagopa.payments.iuvgenerator.IuvCodeBusiness;
import it.gov.pagopa.payments.repository.DebitorRepository;
import it.gov.pagopa.payments.repository.IncrementalIuvNumberRepository;
import it.gov.pagopa.payments.repository.PaymentPositionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StreamUtils;

import com.fasterxml.jackson.core.type.TypeReference;
import com.opencsv.CSVWriter;
import com.opencsv.ICSVWriter;
import com.opencsv.bean.StatefulBeanToCsv;
import com.opencsv.bean.StatefulBeanToCsvBuilder;
import com.opencsv.exceptions.CsvDataTypeMismatchException;
import com.opencsv.exceptions.CsvRequiredFieldEmptyException;

import it.gov.pagopa.commons.model.PaDto;
import it.gov.pagopa.payments.entity.Debitor;
import it.gov.pagopa.payments.entity.IncrementalIuvNumber;
import it.gov.pagopa.payments.entity.PaymentOptions;
import it.gov.pagopa.payments.entity.PaymentPosition;
import it.gov.pagopa.payments.entity.Transfers;
import it.gov.pagopa.payments.generate.debtposition.DebtPositionGeneration;
import it.gov.pagopa.payments.generate.debtposition.bean.DebtPosition;
import it.gov.pagopa.payments.generate.debtposition.bean.debtposition.DPPayer;
import it.gov.pagopa.payments.generate.debtposition.bean.debtposition.DPPaymentDetail;
import it.gov.pagopa.payments.generate.debtposition.bean.debtposition.DPSinglePaymentDetail;
import it.gov.pagopa.payments.generate.paymentnotice.PaymentNoticeGeneration;
import it.gov.pagopa.payments.generate.paymentnotice.bean.PNCreditorInstitution;
import it.gov.pagopa.payments.generate.receipt.PdfReceiptCreator;
import it.gov.pagopa.payments.generate.rpt.xsd.StTipoIdentificativoUnivocoPersFG;
import it.gov.pagopa.payments.model.CsvNotificationModel;
import it.gov.pagopa.payments.model.FilterModel;
import it.gov.pagopa.payments.model.PaymentJobMinimalModel;
import it.gov.pagopa.payments.repository.specification.PaymentPositionWithDateFrom;
import it.gov.pagopa.payments.repository.specification.PaymentPositionWithDateTo;
import it.gov.pagopa.payments.repository.specification.PaymentPositionWithFiscalCode;
import it.gov.pagopa.payments.repository.specification.PaymentPositionWithStatus;
import it.gov.pagopa.payments.repository.specification.PaymentPositionWithTextSearch;

@Service
public class PaymentService {

	@Autowired
	private DebitorRepository debitorRepository;

	@Autowired
    IncrementalIuvNumberRepository incrementalIuvNumberRepository;

	@Autowired
	private PaymentPositionRepository paymentPositionRepository;

	@Autowired
	private PaymentOptionsRepository paymentOptionsRepository;

	@Value("${iuv.generate.auxDigit}")
	private Integer auxDigit;

	@Value("${iuv.generate.segregationCode}")
	private Integer segregationCode;

	@Value("${mailing.csv.mailingType}")
	private String mailingType;

	@Value("${mailing.csv.printMode}")
	private String printMode;

	@Value("${mailing.csv.printType}")
	private String printType;

	@Value("${mailing.csv.billingType}")
	private String billingType;

	@Value("${mailing.csv.costCenter}")
	private String costCenter;

	private String zone = "Europe/Paris";

	public PaymentJobMinimalModel create(List<Debitor> debitors) {

		Long jobId = null;
		int nRecordFound = 0;
		int nRecordAdded = 0;
		int nRecordWarning = 0;

		for (Debitor debitor : debitors) {
			Debitor newDebitor = debitorRepository.findByFiscalCode(debitor.getFiscalCode());
			if (newDebitor == null) {
				newDebitor = copyDebitor(debitor);
			}
			int nRecordAddedPre = nRecordAdded;
			int nRecordWarningPre = nRecordWarning;
			try {
				for (PaymentPosition position : debitor.getPaymentPosition()) {
					jobId = position.getJobId();
					nRecordFound++;

					Boolean check = checkDuplicatePayment(debitor.getFiscalCode(), position);
					nRecordAdded++;
					addNotificationNumber(position.getOrganizationFiscalCode(), position.getPaymentOptions());
					position.setDebitor(null);
					position.setJobId(jobId);
					if (!check.booleanValue()) {
						nRecordWarning++;
						position.setInformation("POSSIBLE_DUPLICATE");
					}

					PaymentPosition pos = paymentPositionRepository.saveAndFlush(position);

					newDebitor.addPaymentPosition(pos);

				}
				if (!newDebitor.getPaymentPosition().isEmpty()) {

					debitorRepository.saveAndFlush(newDebitor);
				}
			} catch (Exception ee) {
				nRecordAdded = nRecordAddedPre;
				nRecordWarning = nRecordWarningPre;
			}

		}

		PaymentJobMinimalModel p = new PaymentJobMinimalModel();
		p.setJobId(jobId);
		p.setNRecordFound(nRecordFound);
		p.setNRecordAdded(nRecordAdded);
		p.setNRecordWarning(nRecordWarning);
		p.setStatus(nRecordFound == nRecordAdded ? JobStatusEnum.SUCCESSO.getStatus() : JobStatusEnum.PARZIALE.getStatus());
		return p;
	}

	private void addNotificationNumber(String creditorFiscalCode, List<PaymentOptions> paymentsOptions) {
		if (paymentsOptions != null) {
			for (PaymentOptions paymentOptions : paymentsOptions) {
				paymentOptions.setNotificationCode(generateNotificationCode(creditorFiscalCode));
			}
		}

	}

	private String generateNotificationCode(String idDominioPa) {
		Long lastNumber = 1l;
		IncrementalIuvNumber incrementalIuvNumber = incrementalIuvNumberRepository.findByIdDominioPaAndAnno(idDominioPa,
				LocalDateTime.now(ZoneId.of(this.zone)).getYear());
		if (incrementalIuvNumber != null) {
			lastNumber = (incrementalIuvNumber.getLastUsedNumber() + 1);
			incrementalIuvNumber.setLastUsedNumber(lastNumber);

		} else {

			incrementalIuvNumber = new IncrementalIuvNumber();
			incrementalIuvNumber.setAnno(LocalDateTime.now(ZoneId.of(this.zone)).getYear());
			incrementalIuvNumber.setIdDominioPa(idDominioPa);
			incrementalIuvNumber.setLastUsedNumber(lastNumber);
		}
		incrementalIuvNumberRepository.saveAndFlush(incrementalIuvNumber);

		IuvCodeGenerator iuvCodeGenerator = new IuvCodeGenerator.Builder().setAuxDigit(auxDigit)
				.setSegregationCode(segregationCode).build();

		IuvCodeBusiness.validate(iuvCodeGenerator);
		return auxDigit + IuvCodeBusiness.generateIUV(segregationCode, lastNumber + "");

	}

	private Boolean checkDuplicatePayment(String debitorFiscalCode, PaymentPosition position) {
		long count = debitorRepository.countByFiscalCodeAndPaymentPositionOrganizationFiscalCodeAndPaymentPositionAmount(
				debitorFiscalCode, position.getOrganizationFiscalCode(), position.getAmount());
		return count == 0;
	}

	private Debitor copyDebitor(Debitor debitor) {
		Debitor newDebitor = new Debitor();
		newDebitor.setAddress(debitor.getAddress());
		newDebitor.setArea(debitor.getArea());
		newDebitor.setCap(debitor.getCap());
		newDebitor.setCountry(debitor.getCountry());
		newDebitor.setEmail(debitor.getEmail());
		newDebitor.setFiscalCode(debitor.getFiscalCode());
		newDebitor.setId(debitor.getId());
		newDebitor.setIdTenant(debitor.getIdTenant());
		newDebitor.setName(debitor.getName());
		newDebitor.setNumber(debitor.getNumber());
		newDebitor.setPhone(debitor.getPhone());
		newDebitor.setProvince(debitor.getProvince());
		newDebitor.setSurname(debitor.getSurname());
		newDebitor.setType(debitor.getType());

		return newDebitor;
	}

	public Page<PaymentPosition> getPaymentsByFilters(String fiscalCode, FilterModel filters, Pageable pageable) {
		if (filters == null) {
			filters = new FilterModel();
		}
		LocalDateTime dateFromTime = null;
		LocalDateTime dateToTime = null;
		LocalDate dateFrom = filters.getDateFrom();
		LocalDate dateTo = filters.getDateTo();
		if (dateFrom != null) {
			dateFromTime = dateFrom.atStartOfDay();
		}
		if (dateTo != null) {
			dateToTime = dateTo.atTime(23, 59, 59, 999999999);
		}
		Specification<PaymentPosition> spec = Specification.where(new PaymentPositionWithDateFrom(dateFromTime))
				.and(new PaymentPositionWithDateTo(dateToTime)).and(new PaymentPositionWithFiscalCode(fiscalCode))
				.and(new PaymentPositionWithStatus(filters.getStatus()))
				.and(new PaymentPositionWithTextSearch(filters.getTextSearch()));
		return paymentPositionRepository.findAll(spec, pageable);
	}

	public PaymentPosition getPaymentByPaymentPositionId(Long id) {
		Optional<PaymentPosition> res = paymentPositionRepository.findById(id);
		PaymentPosition paymentPos = null;
		if (res.isPresent()) {
			paymentPos = res.get();
		}
		return paymentPos;
	}

	public List<PaymentPosition> getPaymentsByJobId(Long jobId) {
		return paymentPositionRepository.findAllByJobId(jobId);
	}

	@Transactional
	public Boolean updatePublishPayment(Long id, LocalDate publishDate) {
		Optional<PaymentPosition> paymentOptional = paymentPositionRepository.findById(id);
		PaymentPosition payment;
		if (paymentOptional.isPresent()) {
			payment = paymentOptional.get();
			if (payment.getStatus().equals(PaymentStatusEnum.BOZZA.getStatus())) {
				payment.setStatus(PaymentStatusEnum.PUBBLICATO.getStatus());
				payment.setPublishDate(publishDate);
				paymentPositionRepository.saveAndFlush(payment);
				return Boolean.TRUE;
			}
		}
		return Boolean.FALSE;
	}

	@Transactional
	public Boolean reportingOptionsPayment(String idOption, String idFlow, LocalDate dateFlow) {
		Optional<PaymentOptions> paymentOptional = paymentOptionsRepository.findByNotificationCode(idOption);
		PaymentOptions option;
		if (paymentOptional.isPresent()) {
			option = paymentOptional.get();
			if (option.getStatus().equals(PaymentOptionStatusEnum.PAGATO.getStatus())) {

				/**
				 * Update Option Status to RENDICONTATO - 3
				 * */			
				option.setIdFlowReporting(idFlow);
				option.setDateReporting(dateFlow);
				option.setStatus(PaymentOptionStatusEnum.RENDICONTATO.getStatus());

				/**
				 * Update Position Status to RENDICONTATO - 6 if:
				 * 
				 * 		1. option is the only refered to the position (isConclusive == true);
				 * 
				 * 		2. option is the last refered to the position (reportedOptionUpdated ==
				 * 		   totalOption - 1).
				 * otherwise update Position Status to RENDICONTATO__PARZIALE - 5
				 */
				Integer reportedOptionUpdated = option.getPaymentPosition().getReportedOptions() + 1;
				Integer totalOption = option.getPaymentPosition().getTotalOptions();
				option.getPaymentPosition().setReportedOptions(reportedOptionUpdated);
				Integer positionStatus = Boolean.TRUE.equals(option.getIsConclusive())
						|| reportedOptionUpdated.compareTo(totalOption - 1) == 0 ? PaymentStatusEnum.RENDICONTATO.getStatus()
								: PaymentStatusEnum.RENDICONTATO_PARZIALE.getStatus();
				option.getPaymentPosition().setStatus(positionStatus);

				paymentOptionsRepository.saveAndFlush(option);
				return Boolean.TRUE;
			}
		}
		return Boolean.FALSE;
	}



	public Boolean deletePayment(Long paymentId, Integer status) {
		Boolean result = Boolean.FALSE;
		PaymentPosition pp = paymentPositionRepository.findByIdAndStatus(paymentId, status);
		if (pp != null) {
			paymentPositionRepository.delete(pp);
			result = Boolean.TRUE;
		}
		return result;
	}

	public List<PaymentPosition> getPaymentPositionsByIds(List<Long> ids) {
		return paymentPositionRepository.findAllById(ids);
	}

	public byte[] generatePaymentNotice(List<PaymentPosition> paymentPositions, PaDto paDto, ZipOutputStream zos)
			throws IOException, CsvDataTypeMismatchException, CsvRequiredFieldEmptyException {
		byte[] bb = null;
		ByteArrayOutputStream baos = null;
		OutputStreamWriter osw = null;
		CSVWriter writer2 = null;
		StatefulBeanToCsv<CsvNotificationModel> writer = null;
		if (zos != null) {
			baos = new ByteArrayOutputStream();
			osw = new OutputStreamWriter(baos);
			writer2 = new CSVWriter(osw, ";".charAt(0), ICSVWriter.NO_QUOTE_CHARACTER, ICSVWriter.DEFAULT_ESCAPE_CHARACTER,
					ICSVWriter.DEFAULT_LINE_END);

			writer = new StatefulBeanToCsvBuilder<CsvNotificationModel>(writer2).withSeparator(';')
					.withApplyQuotesToAll(false).withQuotechar(ICSVWriter.NO_QUOTE_CHARACTER).withOrderedResults(false).build();
		}
		String documentNumber = "docNumber0001";
		byte[] logoData = null;

		InputStream is = TypeReference.class.getResourceAsStream("/media/transparent-logo.png");
		logoData = StreamUtils.copyToByteArray(is);

		String ciName = paDto.getComune();
		String ciFiscalCode = paDto.getCodiceFiscale();
		String ciWebsite = paDto.getSitoIstituzionale();

		String ciSector = "";

		String ciInfo = paDto.getIndirizzo();
		String ciCbillCode = "";
		String ciPostalAccountHolder = null;
		String ciPostalAccountNumber = null;
		String ciPostalAuthorizationCode = null;

		if (!paymentPositions.isEmpty()) {
			ciSector = paymentPositions.get(0).getDescription();
			ciCbillCode = paDto.getCodiceInterbancario();
		}
		PNCreditorInstitution creditorInstitution = PaymentNoticeGeneration.generateCreditorInstitution(logoData, ciName,
				ciSector, ciInfo, ciFiscalCode, ciCbillCode, ciPostalAccountHolder, ciPostalAccountNumber,
				ciPostalAuthorizationCode, ciWebsite);

		int i = 0;
		for (PaymentPosition paymentPosition : paymentPositions) {
			i++;
			String postalIban = getPostalInfo(paymentPositions.get(0), 2);
			Debitor debitor = paymentPosition.getDebitor();
			String pdfFileName = "avviso-" + debitor.getFiscalCode() + "-" + i + ".pdf";

			List<DebtPosition> debtPositionList = new LinkedList<>();

			int installmentNumber = 0;
			boolean isConclusive = false;
			for (PaymentOptions paymentOption : paymentPosition.getPaymentOptions()) {

				if (paymentOption.getIsConclusive().booleanValue()) {
					debtPositionList.add(generateDebtPosition(paymentPosition.getDebitor(), paymentOption, documentNumber, 0,
							paymentPosition.getOrganizationFiscalCode(), postalIban));
					isConclusive = true;
				} else {
					installmentNumber++;
					debtPositionList.add(generateDebtPosition(paymentPosition.getDebitor(), paymentOption, documentNumber,
							installmentNumber, paymentPosition.getOrganizationFiscalCode(), postalIban));
				}
			}
			bb = PaymentNoticeGeneration.generate(debtPositionList, creditorInstitution, true);
			if (zos != null) {
				ZipEntry entry = new ZipEntry(pdfFileName);
				zos.putNextEntry(entry);
				zos.write(bb);
				zos.flush();
				zos.closeEntry();

				addCsvFile(writer, pdfFileName, debitor, paDto, isConclusive, installmentNumber);
				osw.flush();
			}

		}
		if (zos != null) {
			ZipEntry entry = new ZipEntry("indice.csv");
			zos.putNextEntry(entry);
			zos.write(baos.toByteArray());
			zos.flush();
			zos.closeEntry();

			zos.finish();
			zos.flush();

			writer2.close();
			baos.close();
			osw.close();
		}
		return bb;
	}

	private void addCsvFile(StatefulBeanToCsv<CsvNotificationModel> writer, String pdfFileName, Debitor debitor,
			PaDto paDto, boolean isConclusive, int installmentNumber)
			throws CsvDataTypeMismatchException, CsvRequiredFieldEmptyException {
		int finalPage = this.finalPageCalculation(installmentNumber);
		int initialPage = isConclusive ? 1 : 2;

		CsvNotificationModel cnm = new CsvNotificationModel();
		cnm.setFileName(pdfFileName);
		cnm.setMailingType(this.mailingType);
		cnm.setDebitorName(debitor.getName() + " " + debitor.getSurname());
		cnm.setDebitorAddress(debitor.getAddress());
		cnm.setDebitorCap(debitor.getCap());
		cnm.setDebitorLocality(debitor.getArea());
		cnm.setDebitorProvince(debitor.getProvince());
		cnm.setDebitorNation(debitor.getCountry());
		cnm.setCreditorName(paDto.getComune());
		cnm.setCreditorAddress(paDto.getIndirizzo());
		cnm.setCreditorCap(paDto.getCap());
		cnm.setCreditorLocality(paDto.getComune());
		cnm.setCreditorProvince(paDto.getProvincia());
		cnm.setCreditorNation("IT");
		cnm.setFrontages(finalPage);
		cnm.setPagePostalFrom(initialPage);
		cnm.setPagePostalTo(finalPage);
		cnm.setPrintMode(this.printMode);
		cnm.setPrintType(this.printType);
		cnm.setBillingType(this.billingType);
		cnm.setCostCenter(this.costCenter);

		writer.write(cnm);
	}

	private int finalPageCalculation(int installmentNumber) {
		List<Integer> pageNumberTotal = new ArrayList<>(Arrays.asList(1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5));
		return pageNumberTotal.get(installmentNumber);
	}

	private String getPostalInfo(PaymentPosition paymentPosition, int type) {
		String result = null;
		if (paymentPosition != null && paymentPosition.getPaymentOptions() != null
				&& !paymentPosition.getPaymentOptions().isEmpty()
				&& paymentPosition.getPaymentOptions().get(0).getTransfers() != null
				&& !paymentPosition.getPaymentOptions().get(0).getTransfers().isEmpty()) {

			for (Transfers tt : paymentPosition.getPaymentOptions().get(0).getTransfers()) {
				if (tt.getOrganizationFiscalCode().equals(paymentPosition.getOrganizationFiscalCode())) {
					result = this.checkPostal(type, tt);
				}
			}
		}

		return result;

	}

	private String checkPostal(int type, Transfers tt) {
		String result = null;
		if (type == 1) {
			result = tt.getPostalIbanHolder();
		} else if (type == 2) {
			String postalIban = tt.getPostalIban();
			if (postalIban != null && postalIban.length() >= 12) {
				result = postalIban;
			} else {
				result = null;
			}
		} else if (type == 3) {
			result = tt.getPostalAuthCode();
		}
		return result;
	}

	private DebtPosition generateDebtPosition(Debitor debitor, PaymentOptions paymentOption, String documentNumber,
			int installmentNumber, String fiscalCodeEc, String postalIban) {
		StTipoIdentificativoUnivocoPersFG payerUniqueIdentificationType = StTipoIdentificativoUnivocoPersFG.F;
		if (debitor.getType() == 2)
			payerUniqueIdentificationType = StTipoIdentificativoUnivocoPersFG.G;

		LocalDate duoData = paymentOption.getDuoDate();
		String notificationCode = paymentOption.getNotificationCode();
		String iban = null;
		String reason = null;
		BigDecimal amount = paymentOption.getAmount();

		List<Transfers> transfers = paymentOption.getTransfers();
		for (Transfers transfer : transfers) {
			if (fiscalCodeEc.equals(transfer.getOrganizationFiscalCode())) {
				if (postalIban == null) {
					iban = transfer.getIban();
				} else {
					iban = postalIban;
				}
				reason = transfer.getReason();
			}
		}

		Date expirationDate = Date.from(duoData.atStartOfDay(ZoneId.of(this.zone)).toInstant());

		DPPayer payer = DebtPositionGeneration.generatePayer(debitor.getFiscalCode(), payerUniqueIdentificationType,
				debitor.getSurname() + " " + debitor.getName(), debitor.getAddress(), debitor.getNumber(), debitor.getArea(),
				debitor.getProvince(), null, debitor.getCap(), null, null);
		DPPaymentDetail paymentDetail = DebtPositionGeneration.generatePaymentDetail(fiscalCodeEc, auxDigit,
				segregationCode, null, null, null, amount, reason, expirationDate, null, documentNumber, installmentNumber,
				iban, null);
		DPSinglePaymentDetail singlePaymentDetail = DebtPositionGeneration.generateSinglePaymentDetail(amount, 1, reason,
				null, null, null, null, null);
		List<DPSinglePaymentDetail> singlePaymentDetailList = new LinkedList<>();
		singlePaymentDetailList.add(singlePaymentDetail);

		DebtPosition debtPosition = DebtPositionGeneration.generate(payer, paymentDetail, singlePaymentDetailList);
		debtPosition.getPaymentDetail().setNoticeNumberManual(notificationCode);
		return debtPosition;

	}

	public byte[] generateReceipt(PaymentPosition paymentPosition, PaDto enteCreditoreMinimalDto) throws IOException {
		PdfReceiptCreator pdfReceiptCreator = new PdfReceiptCreator(paymentPosition, enteCreditoreMinimalDto);
		pdfReceiptCreator.createDocument();
		pdfReceiptCreator.closeStreamsAndDocument();
		return pdfReceiptCreator.getDocumentInBytes();
	}
}
