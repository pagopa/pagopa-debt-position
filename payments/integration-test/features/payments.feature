Feature: All about Payments

  Background:
    Given Payments running
    And GPS running
    And GPD running
    And DonationService running
    And IUV Generator running
    And ApiConfig running

  Scenario: call donation service
    Given the service "12345" for donations
    And the creditor institution "77777777777" enrolled to donation service "12345"
    When the client sends the DemandPaymentNoticeRequest
    Then the client receives status code 200
    And the client retrieves the amount in the response

  Scenario: call donation service without amount
    Given the service "12345" for donations
    And the creditor institution "77777777777" enrolled to donation service "12345"
    When the client sends a wrong DemandPaymentNoticeRequest
    Then the client receives status code 200
    And the client receives a KO in the response

