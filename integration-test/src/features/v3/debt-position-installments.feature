Feature: Multi installment Debt Position payment using v3 API

  Background:
    Given GPD running

  # Multi Installment Debt Position creation and partial payment
  # (Single Option + Multi Installment Option)
  Scenario: Pay a Debt Position with multiple Payment Options and multiple Installments using v3 API
    Given a random iupd
    When the multi debt position is created using "v3" API with the following installments:
      | option  | notice_number  |
      | 1       | random         |
      | 1       | random         |
      | 2       | random         |
    Then the debt position gets the status code 201
    # Pay Payment Option 1 Installment 1
    When the installment 1 of payment option 1 is paid
    Then the payment option gets the status code 200
    # # Try to Pay Payment Option 2
    When we get the installment 1 of payment option 2
    Then the payment option gets the status code 409
    # # Get Debt Position and check status
    When we get the debt position using "v3" API
    Then the debt position gets the status code 200
    And the debt position gets status "PARTIALLY_PAID"
    # # Try to Pay Payment Option 1 Installment 1
    When we get the installment 2 of payment option 1
    Then the payment option gets the status code 200


  # Multi Installment Debt Position creation and partial payment
  # (2 * Multi Installment Option)
  # todo re-enable when new version of GPD (compliat to multi installments) will be available
  # Scenario: Pay a Debt Position with multiple Payment Options and multiple Installments using v3 API
  #   Given a random iupd
  #   When the multi debt position is created using "v3" API with the following installments:
  #     | option  | notice_number  |
  #     | 1       | random         |
  #     | 1       | random         |
  #     | 2       | random         |
  #     | 2       | random         |
  #   Then the debt position gets the status code 201
  #   # Pay Payment Option 1 Installment 1
  #   When the installment 1 of payment option 1 is paid
  #   Then the payment option gets the status code 200
  #   # # Try to Pay Payment Option 2
  #   When we get the installment 1 of payment option 2
  #   Then the payment option gets the status code 409
  #   # # Get Debt Position and check status
  #   When we get the debt position using "v3" API
  #   Then the debt position gets the status code 200
  #   And the debt position gets status "PARTIALLY_PAID"
  #   # # Try to Pay Payment Option 1 Installment 1
  #   When we get the installment 2 of payment option 1
  #   Then the payment option gets the status code 200
