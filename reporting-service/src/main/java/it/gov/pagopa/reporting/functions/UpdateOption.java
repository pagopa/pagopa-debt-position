package it.gov.pagopa.reporting.functions;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.azure.functions.ExecutionContext;
import com.microsoft.azure.functions.annotation.FunctionName;
import com.microsoft.azure.functions.annotation.QueueTrigger;
import it.gov.pagopa.reporting.models.OptionsMessage;
import it.gov.pagopa.reporting.models.PaymentOption;
import it.gov.pagopa.reporting.models.RetryStep;
import it.gov.pagopa.reporting.service.GPDService;
import it.gov.pagopa.reporting.service.OptionsService;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

/**
 * Azure Functions with Azure Queue trigger.
 */
public class UpdateOption {

    private final String maxAttempts = System.getenv("MAX_ATTEMPTS");
    private final String storageConnectionString = System.getenv("FLOW_SA_CONNECTION_STRING");
    private final String optionsQueue = System.getenv("OPTIONS_QUEUE");


    /**
     * This function will be invoked when a new message is detected in the queue
     */
    @FunctionName("UpdateOptionFunction")
    public void run(@QueueTrigger(name = "UpdateOptionTrigger", queueName = "%OPTIONS_QUEUE%", connection = "FLOW_SA_CONNECTION_STRING") String message, final ExecutionContext context) {

        Logger logger = context.getLogger();

        try {
            var invocationId = context.getInvocationId();
            logger.log(Level.INFO, () -> "[UpdateOptionFunction START][id=" + invocationId + "]  processed a message " + message);

            OptionsMessage options = new ObjectMapper().readValue(message, OptionsMessage.class);

            logger.log(Level.INFO, () -> "[UpdateOptionFunction][id=" + invocationId + "] Update flow " + options.getIdFlow() + " with data " + options.getFlowDate());

            GPDService gpdService = this.getGPDServiceInstance();
            var failed = options.getPaymentOptions()
                    .stream()
                    .filter(paymentOption -> {
                        var retryStep = gpdService.setReport(options.getIdPA(), paymentOption, logger, invocationId);
                        paymentOption.setRetryAction(retryStep.name());
                        return !RetryStep.DONE.equals(retryStep);
                    })
                    .collect(Collectors.toList());

            if (!failed.isEmpty()) {
                handleFailedRows(logger, options, failed, invocationId);
            }
            logger.log(Level.INFO, () -> "[UpdateOptionFunction END]  processed a message " + message);
        } catch (JsonProcessingException em) {

            logger.log(Level.SEVERE, () -> "[UpdateOptionFunction Error] Invalid Message Queue " + em.getMessage() + " - message " + message);
        } catch (Exception e) {

            logger.log(Level.SEVERE, () -> "[UpdateOptionFunction Error] Generic Error " + e.getMessage() + " " + e.getCause() + " - message " + message);
        }

    }

    private void handleFailedRows(Logger logger, OptionsMessage options, List<PaymentOption> failed, String invocationId) {
        int maxRetry = getMaxRetry();

        // if elem is failed with 4xx HTTP status code, it isn't retryable
        var notRetryable = failed.stream()
                .filter(elem -> RetryStep.ERROR.name().equals(elem.getRetryAction()))
                .collect(Collectors.toList());

        var retryable = failed.stream()
                .filter(elem -> !RetryStep.ERROR.name().equals(elem.getRetryAction()))
                .collect(Collectors.toList());

        // retry only if maxRetry is not reached
        if (!retryable.isEmpty() && options.getRetryCount() < maxRetry) {
            retryable.forEach(elem -> logger.log(Level.WARNING, () -> String.format(
                    "[id=%s][GPD Error][setReport] Retry for RENDICONTATO iuv : %s , transfer: %s", invocationId, elem.getOptionId(), elem.getTransferId())));
            // insert message in queue
            var queueService = getOptionQueueService(logger);
            OptionsMessage message = new OptionsMessage();
            message.setIdPA(options.getIdPA());
            message.setIdFlow(options.getIdFlow());
            message.setFlowDate(options.getFlowDate());
            message.setPaymentOptions(retryable);
            message.setRetryCount(options.getRetryCount() + 1);
            queueService.insertMessage(message);
        } else {
            // stop retry at max attempts
            notRetryable.addAll(retryable);
        }
        if (!notRetryable.isEmpty()) {
            // log ERROR for alerting
            notRetryable.forEach(elem -> logger.log(Level.SEVERE, () -> String.format(
                    "[id=%s][UpdateOptionFunction Error] can't update iuv : %s , transfer: %s", invocationId, elem.getOptionId(), elem.getTransferId())));
        }
    }

    protected int getMaxRetry() {
        return maxAttempts != null ? Integer.parseInt(maxAttempts) : 0;
    }

    public GPDService getGPDServiceInstance() {
        return GPDService.getInstance();
    }

    protected OptionsService getOptionQueueService(Logger logger) {
        return new OptionsService(this.storageConnectionString, this.optionsQueue, logger);
    }
}
