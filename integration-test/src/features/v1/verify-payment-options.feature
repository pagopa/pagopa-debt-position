@verifyPaymentOptions
Feature: Verify payment options by NAV
  As an EC
  I want to verify all payable alternatives associated to a notice number (NAV)
  So that I can present the customer either a single-payment option or one of the available plans

  Background:
    Given GPD running

  Scenario: 200 - grouping single and plan, descriptions and ordering
    Given a V3 debt position with one single-payment and one 2-installment plan exists for organization "700123456789001"
    When I call verifyPaymentOptions for organization "700123456789001" with the single installment nav
    Then the HTTP status is 200
    And the payload has at least 2 payment option groups
    And there is at least one group with 1 installment described as "Pagamento in un'unica soluzione"
    And there is at least one group with more than 1 installment described starting with "Piano rateale"
    And groups are ordered by ascending dueDate

  Scenario: 404 - NAV not found
    When I call verifyPaymentOptions for organization "700123456789001" with nav "999999999999999"
    Then the HTTP status is 404

  Scenario: 400 - malformed NAV
    When I call verifyPaymentOptions for organization "700123456789001" with nav "ABCDEF"
    Then the HTTP status is 400
    And the response content-type contains "application/json"