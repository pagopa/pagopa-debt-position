package it.gov.pagopa.debtposition.exception;

import lombok.Getter;

import java.util.EnumMap;
import java.util.Map;

import org.springframework.http.HttpStatus;

@Getter
public enum AppError {
	DEBT_POSITION_REQUEST_DATA_ERROR(
		      HttpStatus.BAD_REQUEST, "Error in the debt position request data", "%s"),
		  DEBT_POSITION_CREATION_FAILED(
		      HttpStatus.INTERNAL_SERVER_ERROR,
		      "The debt position creation is failed",
		      "Creation failed for the debt position with Organization Fiscal Code %s"),
		  DEBT_POSITION_UPDATE_FAILED(
		      HttpStatus.INTERNAL_SERVER_ERROR,
		      "The debt position update is failed",
		      "Update failed for the debt position with Organization Fiscal Code %s"),
		  DEBT_POSITION_DELETE_FAILED(
		      HttpStatus.INTERNAL_SERVER_ERROR,
		      "The debt position delete is failed",
		      "Delete failed for the debt position with Organization Fiscal Code %s"),
		  DEBT_POSITION_UNIQUE_VIOLATION(
		      HttpStatus.CONFLICT,
		      "The debt position violated constraints of uniqueness",
		      "Already exists a debt position for the Organization Fiscal Code %s or one of its payment"
		          + " options is not unique"),
		  DEBT_POSITION_UPDATE_FAILED_NO_TRANSFER_FOR_NOTIFICATION_FEE(
		      HttpStatus.UNPROCESSABLE_ENTITY,
		      "The debt position update is failed",
		      "No valid transfer found to apply the registered notification fee on the debt position with"
		          + " Organization Fiscal Code %s and IUPD %s"),
		  DEBT_POSITION_NOT_FOUND(
		      HttpStatus.NOT_FOUND,
		      "Not found the debt position",
		      "Not found a debt position for Organization Fiscal Code %s and IUPD %s"),
		  DEBT_POSITION_IN_UPDATABLE_STATE_NOT_FOUND(
		      HttpStatus.NOT_FOUND,
		      "No debt position in updatable state found",
		      "Not found a debt position in updatable state for Organization Fiscal Code %s"),
		  DEBT_POSITION_PAYMENT_FOUND(
		      HttpStatus.CONFLICT,
		      "Existing related payment found",
		      "A payment transaction has already been carried out on the debt position with Organization"
		          + " Fiscal Code %s and IUPD %s"),
		  DEBT_POSITION_NOT_UPDATABLE(
		      HttpStatus.CONFLICT,
		      "Existing related payment found or not in updatable state",
		      "A payment transaction has already been carried out or, the debt position with Organization"
		          + " Fiscal Code %s and IUPD %s, is not in updatable state"),
		  DEBT_POSITION_NOT_PUBLISHABLE(
		      HttpStatus.CONFLICT,
		      "Existing related payment found or not in publishable state",
		      "A payment transaction has already been carried out or, the debt position with Organization"
		          + " Fiscal Code %s and IUPD %s, is not in publishable state"),
		  DEBT_POSITION_PUBLISH_VALIDITY_DATE_MISMATCH(
		      HttpStatus.CONFLICT,
		      "Publish request occurred after the validity date began",
		      "A publish request has been made after the validity date began for the debt position with"
		          + " Organization Fiscal Code %s and IUPD %s"),
		  DEBT_POSITION_PUBLISH_DUE_DATE_MISMATCH(
		      HttpStatus.CONFLICT,
		      "Publish request occurred after the due date of a payment options has expired",
		      "A publish request occurred after the due date of a payment options has expired for the debt"
		          + " position with Organization Fiscal Code %s and IUPD %s"),
      DEBT_POSITION_PUBLISH_DUE_DATE_BEFORE_VALIDITY_DATE(
              HttpStatus.CONFLICT,
              "The due date of a payment option is before the validity date",
              "Invalid publish request: the payment option due date cannot be before the validity date for the debt"
                      + " position with Organization Fiscal Code %s and IUPD %s"),
		  DEBT_POSITION_PUBLISH_FAILED(
		      HttpStatus.INTERNAL_SERVER_ERROR,
		      "The debt position publish is failed",
		      "Publish failed for the debt position with Organization Fiscal Code %s and IUPD %s"),
		  DEBT_POSITION_INVALIDATE_FAILED(
		      HttpStatus.INTERNAL_SERVER_ERROR,
		      "The debt position invalidate is failed",
		      "Invalidate failed for the debt position with Organization Fiscal Code %s and IUPD %s"),
		  DEBT_POSITION_NOT_INVALIDABLE(
		      HttpStatus.CONFLICT,
		      "Existing related payment found or not in invalidable state",
		      "A payment transaction has already been carried out or, the debt position with Organization"
		          + " Fiscal Code %s and IUPD %s, is not in invalidable state"),
		  DEBT_POSITION_NOT_RECOVERABLE(
		      HttpStatus.BAD_REQUEST,
		      "The debt positions cannot be recovered",
		      "Debt positions cannot be recovered. Verify that due_date_to >= due_date_from and that the"
		          + " interval between the two dates not exceed the maximum number of days allowed"
		          + " [due_date_from= %s, due_date_to= %s, days_between_from_to= %s, max_days_interval="
		          + " %s]"),
		  DEBT_POSITION_FORBIDDEN(
		      HttpStatus.FORBIDDEN,
		      "The payment position is forbidden",
		      "The caller does not have proper authorization to access or modify the IUVs in the payment"
		          + " position. [Organization Fiscal Code=%s, IUPD=%s]"),
		  DEBT_POSITION_FORBIDDEN_ON_NAV(
		          HttpStatus.FORBIDDEN,
		          "The payment position is forbidden",
		          "The caller does not have proper authorization to access or modify the NAVs in the payment"
		                  + " position. [Organization Fiscal Code=%s]"),
		  PAYMENT_OPTION_RESERVED_METADATA(
		          HttpStatus.CONFLICT,
		          "The payment option contains reserved metadata",
		          "The caller should not add or modify reserved payment option metadata. Reserved metadata list = {NOTIFICATION_FEE}."),
		  ORGANIZATION_NOT_FOUND(
		      HttpStatus.NOT_FOUND,
		      "Not found the organization",
		      "Not found an organization for the Organization Fiscal Code %s"),
		  PAYMENT_OPTION_NOT_FOUND(
		      HttpStatus.NOT_FOUND,
		      "Not found the payment option",
		      "Not found a payment option for Organization Fiscal Code %s and NAV %s"),
		  PAYMENT_OPTION_IUV_NOT_FOUND(
		      HttpStatus.NOT_FOUND,
		      "Not found the payment option",
		      "Not found a payment option for Organization Fiscal Code %s and IUV %s"),
		  PAYMENT_OPTION_NOT_PAYABLE(
		      HttpStatus.UNPROCESSABLE_ENTITY,
		      "Not in payable state",
		      "The payment option with Organization Fiscal Code %s and NAV %s is not in payable state"),
		  PAYMENT_OPTION_ALREADY_PAID(
		      HttpStatus.CONFLICT,
		      "Existing related payment found",
		      "A payment transaction has already been carried out for the Organization Fiscal Code %s and"
		          + " NAV %s"),
		  PAYMENT_OPTION_NOTIFICATION_FEE_UPDATE_FAILED(
		      HttpStatus.INTERNAL_SERVER_ERROR,
		      "The update of the notification fee for the payment option is failed",
		      "Notification fee update failed for the payment option with Organization Fiscal Code %s and"
		          + " IUV %s"),
		  PAYMENT_OPTION_NOTIFICATION_FEE_UPDATE_NOT_UPDATABLE(
		      HttpStatus.UNPROCESSABLE_ENTITY,
		      "Not in unpaid state for notification fee update",
		      "The payment option with Organization Fiscal Code %s and NAV %s is not in unpaid state and"
		          + " the notification fee cannot be updated."),
		  PAYMENT_OPTION_NOTIFICATION_FEE_UPDATE_TRANSFER_NOT_FOUND(
		      HttpStatus.UNPROCESSABLE_ENTITY,
		      "No valid transfer found for notification fee update",
		      "No transfer found for payment option with IUV %s related to Organization Fiscal Code %s"),
		  PAYMENT_OPTION_PAY_FAILED(
		      HttpStatus.INTERNAL_SERVER_ERROR,
		      "The pay call for a payment option is failed",
		      "Payment failed for the payment option with Organization Fiscal Code %s and NAV %s"),
		  TRANSFER_NOT_FOUND(
		      HttpStatus.NOT_FOUND,
		      "Not found the transfer",
		      "Not found a transfer for Organization Fiscal Code %s, IUV %s and TxID %s"),
		  TRANSFER_REPORTING_FAILED(
		      HttpStatus.INTERNAL_SERVER_ERROR,
		      "The reporting for the transfer is failed",
		      "Reporting failed for the transfer with Organization Fiscal Code %s, IUV %s and TxID %s"),
		  TRANSFER_NOT_ACCOUNTABLE(
		      HttpStatus.CONFLICT,
		      "transfer is not in accountable state",
		      "The transfer option with Organization Fiscal Code %s, IUV %s and TxID %s is not in payable"
		          + " state"),
		  UNPROCESSABLE_ENTITY(
			      HttpStatus.UNPROCESSABLE_ENTITY, "The debt position operation is failed", null),
		  V1_UNPROCESSABLE_ENTITY_MULTI_INSTALLMENTS(
		          HttpStatus.UNPROCESSABLE_ENTITY, "MULTI_INSTALLMENT_NOT_SUPPORTED_IN_V1", "Multi-installment positions are not supported " +
		          "by this version of the API (v1). Organization Fiscal Code %s, IUPD %s."),
		  PAYMENT_PLAN_ID_MISSING(
				    HttpStatus.UNPROCESSABLE_ENTITY,
				    "Invalid payment plan",
				    "Installment with isPartialPayment=true must have a non-blank paymentPlanId. IUV=%s, Organization Fiscal Code=%s"
				),
		  UNKNOWN(null, null, null);


	public final HttpStatus httpStatus;
	public final String title;
	public final String details;

	AppError(HttpStatus httpStatus, String title, String details) {
		this.httpStatus = httpStatus;
		this.title = title;
		this.details = details;
	}

	//====== ODP/PAA Mapping ======

	public static final class OdpSpec {
		public final String appErrorCode;  // e.g.: "ODP-107"
		public final HttpStatus httpStatusOverride;
		public final String paaCode;       // e.g.: "PAA_PAGAMENTO_SCONOSCIUTO"
		private OdpSpec(String appErrorCode, HttpStatus httpStatusOverride, String paaCode) {
			this.appErrorCode = appErrorCode;
			this.httpStatusOverride = httpStatusOverride;
			this.paaCode = paaCode;
		}
		public static OdpSpec of(String code, HttpStatus override, String paa) {
			return new OdpSpec(code, override, paa);
		}
	}

	/** ODP overlays */
	private static final Map<AppError, OdpSpec> ODP_OVERLAY = new EnumMap<>(AppError.class);
	static {
		// Table (HTTP, ODP, PAA_*)
		// 400 ODP-101 PAA_SINTASSI
		// 422 ODP-102 PAA_SEMANTICA
		// 500 ODP-103 PAA_SYSTEM_ERROR
		// 400 ODP-104 PAA_ID_DOMINIO_ERRATO
		// 400 ODP-105 PAA_ID_INTERMEDIARIO_ERRATO
		// 400 ODP-106 PAA_STAZIONE_INT_ERRATA
		// 404 ODP-107 PAA_PAGAMENTO_SCONOSCIUTO
		// 409 ODP-108 PAA_PAGAMENTO_DUPLICATO
		// 409 ODP-109 PAA_PAGAMENTO_IN_CORSO
		// 422 ODP-110 PAA_PAGAMENTO_SCADUTO
		// 422 ODP-111 PAA_PAGAMENTO_ANNULLATO

		final String PAA_PAGAMENTO_SCONOSCIUTO = "PAA_PAGAMENTO_SCONOSCIUTO";
		final String PAA_SYSTEM_ERROR = "PAA_SYSTEM_ERROR";
		final String ODP_107 = "ODP-107";
		final String ODP_103 = "ODP-103";

		ODP_OVERLAY.put(PAYMENT_OPTION_NOT_FOUND,
				OdpSpec.of(ODP_107, HttpStatus.NOT_FOUND, PAA_PAGAMENTO_SCONOSCIUTO));
		ODP_OVERLAY.put(PAYMENT_OPTION_IUV_NOT_FOUND,
				OdpSpec.of(ODP_107, HttpStatus.NOT_FOUND, PAA_PAGAMENTO_SCONOSCIUTO));
		ODP_OVERLAY.put(TRANSFER_NOT_FOUND,
				OdpSpec.of(ODP_107, HttpStatus.NOT_FOUND, PAA_PAGAMENTO_SCONOSCIUTO));

		ODP_OVERLAY.put(PAYMENT_OPTION_ALREADY_PAID,
				OdpSpec.of("ODP-108", HttpStatus.CONFLICT, "PAA_PAGAMENTO_DUPLICATO"));
		ODP_OVERLAY.put(TRANSFER_NOT_ACCOUNTABLE,
				OdpSpec.of("ODP-109", HttpStatus.CONFLICT, "PAA_PAGAMENTO_IN_CORSO"));

		ODP_OVERLAY.put(PAYMENT_OPTION_NOT_PAYABLE,
				OdpSpec.of("ODP-110", HttpStatus.UNPROCESSABLE_ENTITY, "PAA_PAGAMENTO_SCADUTO"));

		// 422 Errors
		ODP_OVERLAY.put(UNPROCESSABLE_ENTITY,
				OdpSpec.of("ODP-102", HttpStatus.UNPROCESSABLE_ENTITY, "PAA_SEMANTICA"));
		ODP_OVERLAY.put(PAYMENT_OPTION_NOTIFICATION_FEE_UPDATE_NOT_UPDATABLE,
				OdpSpec.of("ODP-102", HttpStatus.UNPROCESSABLE_ENTITY, "PAA_SEMANTICA"));

		// 500 System error
		ODP_OVERLAY.put(PAYMENT_OPTION_PAY_FAILED,
				OdpSpec.of(ODP_103, HttpStatus.INTERNAL_SERVER_ERROR, PAA_SYSTEM_ERROR));
		ODP_OVERLAY.put(PAYMENT_OPTION_NOTIFICATION_FEE_UPDATE_FAILED,
				OdpSpec.of(ODP_103, HttpStatus.INTERNAL_SERVER_ERROR, PAA_SYSTEM_ERROR));
		ODP_OVERLAY.put(TRANSFER_REPORTING_FAILED,
				OdpSpec.of(ODP_103, HttpStatus.INTERNAL_SERVER_ERROR, PAA_SYSTEM_ERROR));

		// Errors of type 403 are NOT recorded → they are mapped as 404/ODP-107
		ODP_OVERLAY.put(DEBT_POSITION_FORBIDDEN,
				OdpSpec.of(ODP_107, HttpStatus.NOT_FOUND, PAA_PAGAMENTO_SCONOSCIUTO));
		ODP_OVERLAY.put(DEBT_POSITION_FORBIDDEN_ON_NAV,
				OdpSpec.of(ODP_107, HttpStatus.NOT_FOUND, PAA_PAGAMENTO_SCONOSCIUTO));
	}

	/** Returns the ODP spec for the error */
	public OdpSpec odpSpec() {
		return ODP_OVERLAY.get(this);
	}
}
