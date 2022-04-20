package it.gov.pagopa.reporting.models;

/**
 * The step to retry
 */
public enum RetryStep {
    NONE,  // not executed
    RETRY, // to retry
    DONE,  // done
    ERROR  // skip retry
}
