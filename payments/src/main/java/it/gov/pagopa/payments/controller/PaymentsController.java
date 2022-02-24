package it.gov.pagopa.payments.controller;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.stream.Collectors;
import java.util.zip.ZipOutputStream;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import it.gov.pagopa.payments.enumeration.PaymentStatusEnum;
import org.modelmapper.ModelMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;

import com.opencsv.ICSVWriter;
import com.opencsv.bean.StatefulBeanToCsv;
import com.opencsv.bean.StatefulBeanToCsvBuilder;
import com.opencsv.exceptions.CsvDataTypeMismatchException;
import com.opencsv.exceptions.CsvRequiredFieldEmptyException;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import it.gov.pagopa.commons.model.PaDto;
import it.gov.pagopa.payments.entity.Debitor;
import it.gov.pagopa.payments.entity.PaymentPosition;
import it.gov.pagopa.payments.model.BooleanResponseModel;
import it.gov.pagopa.payments.model.CsvPositionModel;
import it.gov.pagopa.payments.model.ExportModel;
import it.gov.pagopa.payments.model.FindModel;
import it.gov.pagopa.payments.model.FindResponseModel;
import it.gov.pagopa.payments.model.OptionsReportingModel;
import it.gov.pagopa.payments.model.PaymentJobMinimalModel;
import it.gov.pagopa.payments.model.PaymentMinimalModel;
import it.gov.pagopa.payments.model.PaymentPositionDetailModel;
import it.gov.pagopa.payments.model.PaymentsModel;
import it.gov.pagopa.payments.model.PublishModel;
import it.gov.pagopa.payments.model.UploadCsvModel;
import it.gov.pagopa.payments.model.UploadCsvPartialModel;
import it.gov.pagopa.payments.model.tribute.TributeServiceModel;
import it.gov.pagopa.payments.service.PaymentService;

@RestController()
@RequestMapping("payments")
public class PaymentsController {
	@Autowired
	PaymentService paymentService;

	@Autowired
	private ModelMapper modelMapper;

	@Autowired
	private RestTemplate restTemplate;

	@Value("${service.service-management.path}")
	private String serviceManagementPath;

	@Value("${service.ente.path}")
	private String entePath;

	private Logger logger = LoggerFactory.getLogger(PaymentsController.class);

	@ApiOperation(value = "Salva la lista dei pagamenti", notes = "Servizio REST per salvare la lista dei pagamenti", response = PaymentJobMinimalModel.class)
	@PostMapping(value = "create")
	public PaymentJobMinimalModel createPayments(
			@ApiParam(value = "Lista dei pagamenti", required = true) @Valid @RequestBody final UploadCsvPartialModel uploadCvsModel) {
		logger.info("POST create payments");
		TributeServiceModel tributeServiceModel = restTemplate.getForObject(
				serviceManagementPath + "/service-management/service/" + uploadCvsModel.getFiscalCodeCreditor(),
				TributeServiceModel.class);
		UploadCsvModel upload = modelMapper.map(uploadCvsModel, UploadCsvModel.class);
		upload.setTributeService(tributeServiceModel);
		PaymentsModel paymentsModel = modelMapper.map(upload, PaymentsModel.class);
		List<Debitor> debitors = paymentsModel.getDebitors().stream()
				.map(paymentModel -> modelMapper.map(paymentModel, Debitor.class)).collect(Collectors.toList());

		return paymentService.create(debitors);
	}

	@ApiOperation(value = "Recupera lista dei pagamenti dato il codice fiscale dell'ente", notes = "Servizio REST per recuperare lista dei pagamenti dato il codice fiscale dell'ente", response = List.class)
	@PostMapping(value = "find")
	public FindResponseModel getPayments(
			@ApiParam(value = "Filtri", required = true) @RequestBody final FindModel findModel) {

		logger.info("POST find payments");

		Pageable paging = PageRequest.of(findModel.getPage(), findModel.getSize(),
				Sort.by("information").ascending().and(Sort.by("insertDate").descending()));
		Page<PaymentPosition> pageResults = paymentService.getPaymentsByFilters(findModel.getFiscalCode(),
				findModel.getFilters(), paging);

		List<PaymentPosition> content = pageResults.getContent();
		List<PaymentMinimalModel> result = content.stream()
				.map(paymentModel -> modelMapper.map(paymentModel, PaymentMinimalModel.class)).collect(Collectors.toList());

		FindResponseModel response = new FindResponseModel();
		response.setCurrentPage(pageResults.getNumber());
		response.setPayments(result);
		response.setTotalItems(pageResults.getTotalElements());
		response.setTotalPages(pageResults.getTotalPages());

		return response;
	}

	@ApiOperation(value = "Recupera il dettaglio di un pagamento dato un paymentPositionId", notes = "Recupera il dettaglio di un pagamento dato un paymentPositionId", response = PaymentPositionDetailModel.class)
	@GetMapping(value = "/info/{id}")
	public PaymentPositionDetailModel getPaymentsByPaymentPositionId(@PathVariable("id") Long id) {
		logger.info("GET info");
		return modelMapper.map(paymentService.getPaymentByPaymentPositionId(id), PaymentPositionDetailModel.class);

	}

	@ApiOperation(value = "Esporta i pagamenti in formato csv dato un jobId", notes = "Esporta i pagamenti in formato csv dato un jobId")
	@GetMapping(value = "/export/{jobId}/{filename}")
	public void exportCsv(@PathVariable("jobId") Long jobId, @PathVariable("filename") String filename,
			HttpServletResponse response) throws CsvDataTypeMismatchException, CsvRequiredFieldEmptyException, IOException {
		logger.info("GET export CSV");

		response.setContentType("text/csv");
		response.setHeader(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + filename + "\"");

		StatefulBeanToCsv<CsvPositionModel> writer = new StatefulBeanToCsvBuilder<CsvPositionModel>(response.getWriter())
				.withQuotechar(ICSVWriter.NO_QUOTE_CHARACTER).withSeparator(ICSVWriter.DEFAULT_SEPARATOR)
				.withOrderedResults(false).build();

		List<PaymentPosition> content = paymentService.getPaymentsByJobId(jobId);
		List<CsvPositionModel> csvPositions = content.stream()
				.map(csvPositionModel -> modelMapper.map(csvPositionModel, CsvPositionModel.class))
				.collect(Collectors.toList());
		// write all users to csv file
		writer.write(csvPositions);

	}

	@ApiOperation(value = "Pubblica i pagamenti selezionati", notes = "Servizio REST per pubblicare i pagamenti selezionati", response = BooleanResponseModel.class)
	@PostMapping(value = "publishPayments")
	public BooleanResponseModel publishPayments(
			@ApiParam(value = "Lista di ID e data", required = true) @Valid @RequestBody final PublishModel publishModel) {
		logger.info("POST publish payments");
		for (Long id : publishModel.getIds()) {
			paymentService.updatePublishPayment(id, publishModel.getPublishDate());
		}
		return new BooleanResponseModel(true);
	}

	@ApiOperation(value = "Cancella un avviso di pagamento", notes = "Servizio REST per cancellare un avviso di pagamento", response = BooleanResponseModel.class)
	@DeleteMapping(value = "/{paymentId}")
	public BooleanResponseModel delete(@PathVariable("paymentId") Long paymentId) {
		logger.info("DELETE delete payment");
		return new BooleanResponseModel(paymentService.deletePayment(paymentId, PaymentStatusEnum.BOZZA.getStatus()));
	}

	@ApiOperation(value = "Genera avvisi pagamento", notes = "Genera avvisi pagamento")
	@PostMapping(value = "/exportPayments")
	public ResponseEntity<Resource> exportPayments(@RequestBody final ExportModel exportModel,
			HttpServletResponse response) throws CsvDataTypeMismatchException, CsvRequiredFieldEmptyException, IOException {
		logger.info("POST exportZip");

		String nomeFile = "export-" + LocalDateTime.now().format(DateTimeFormatter.ofPattern("ddMMyyyykkmmss")) + ".";
		String extension = exportModel.getIsMailing().booleanValue() ? "zip" : "pdf";

		ByteArrayResource resource = null;
		HttpHeaders header = new HttpHeaders();
		header.add(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + nomeFile + extension);
		header.add("Cache-Control", "no-cache, no-store, must-revalidate");
		header.add("Pragma", "no-cache");
		header.add("Expires", "0");

		List<PaymentPosition> paymentPositions = paymentService.getPaymentPositionsByIds(exportModel.getIds());
		if (!paymentPositions.isEmpty()) {
			String fiscalCode = paymentPositions.get(0).getOrganizationFiscalCode();
			if (exportModel.getIsMailing().booleanValue()) {
				resource = this.exportNotificationZip(fiscalCode, paymentPositions);
			} else {
				resource = this.exportNotificationPdf(fiscalCode, paymentPositions);
			}

		}
		if (resource == null) {
			return ResponseEntity.ok().body(null);
		} else {
			return ResponseEntity.ok().headers(header).contentLength(resource.contentLength())
					.contentType(MediaType.parseMediaType("application/" + extension)).body(resource);
		}

	}

	@ApiOperation(value = "Genera il PDF di una ricevuta dato un pagamento", notes = "Genera il PDF di una ricevuta dato un pagamento")
	@GetMapping(value = "/receipt/{paymentId}")
	public ResponseEntity<Resource> generateReceipt(@PathVariable("paymentId") Long paymentId,
			HttpServletResponse response) throws IOException {
		logger.info("GET receipt PDF");
		String filename = "receipt-" + LocalDateTime.now().format(DateTimeFormatter.ofPattern("ddMMyyyykkmmss")) + ".pdf";

		ByteArrayResource resource = null;
		HttpHeaders header = new HttpHeaders();
		header.add(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename);
		header.add("Cache-Control", "no-cache, no-store, must-revalidate");
		header.add("Pragma", "no-cache");
		header.add("Expires", "0");

		PaymentPosition paymentPosition = paymentService.getPaymentByPaymentPositionId(paymentId);
		String fiscalCode = paymentPosition.getOrganizationFiscalCode();
		PaDto enteCreditoreMinimalDto = this.getPa(fiscalCode);
		byte[] pdfByte = paymentService.generateReceipt(paymentPosition, enteCreditoreMinimalDto);
		resource = new ByteArrayResource(pdfByte);

		return ResponseEntity.ok().headers(header).contentLength(resource.contentLength())
				.contentType(MediaType.APPLICATION_PDF).body(resource);

	}

	@ApiOperation(value = "Rendiconta le opzioni di pagamento", notes = "Servizio REST per rendicontare le opzioni", response = BooleanResponseModel.class)
	@PostMapping(value = "/options/reporting")
	public BooleanResponseModel reporting(
			@ApiParam(value = "Lista di ID options legate ad un flusso di rendicontazione", required = true) @Valid @RequestBody final OptionsReportingModel reportingOptionsModel) {
		logger.info("Reporting options");
		boolean result = true;
		for (String notificationCode : reportingOptionsModel.getNotificationCodes()) {
			result = result && paymentService.reportingOptionsPayment(notificationCode, reportingOptionsModel.getIdFlow(),
					reportingOptionsModel.getDateFlow());
		}
		return new BooleanResponseModel(result);
	}

	private ByteArrayResource exportNotificationPdf(String fiscalCode, List<PaymentPosition> paymentPositions)
			throws CsvDataTypeMismatchException, CsvRequiredFieldEmptyException, IOException {

		ByteArrayResource resource = null;

		PaDto paDto = this.getPa(fiscalCode);

		if (paDto != null) {

			byte[] pdfByte = paymentService.generatePaymentNotice(paymentPositions, paDto, null);
			resource = new ByteArrayResource(pdfByte);
		}

		return resource;
	}

	private ByteArrayResource exportNotificationZip(String fiscalCode, List<PaymentPosition> paymentPositions)
			throws CsvDataTypeMismatchException, CsvRequiredFieldEmptyException, IOException {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		ByteArrayResource resource = null;
		try (ZipOutputStream zos = new ZipOutputStream(baos)) {

			PaDto paDto = this.getPa(fiscalCode);

			if (paDto != null) {

				paymentService.generatePaymentNotice(paymentPositions, paDto, zos);
				resource = new ByteArrayResource(baos.toByteArray());
			}

		} finally {
			baos.close();
		}
		return resource;
	}

	private PaDto getPa(String fiscalCode) {
		return restTemplate.getForObject(entePath + "/pa/" + fiscalCode, PaDto.class);
	}
}
