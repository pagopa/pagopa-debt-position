Feature: All about Services

  Background:
    Given Payments running
    And GPS running
    And DonationService running

  Scenario: success services list
    Given the service "12345" for donations
    And the creditor institution "77777777777" enrolled to donation service "12345"
    When the client sends the DemandPaymentNoticeRequest
    Then the client receives status code 200
    And the client retrieves the amount in the response

