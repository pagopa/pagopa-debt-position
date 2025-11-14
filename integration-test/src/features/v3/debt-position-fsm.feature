Feature: Test Finite State Machine of a debt position using v3 API

  Background:
    Given GPD running

  # create DRAFT
  # update PUBLISH
  # update VALID
  Scenario: Debt Position creation from DRAFT to VALID
    Given a random iupd
    When the debt position is created using "v3" API
    And the debt position is updated and published using "v3" API
    Then the debt position gets the status code 200
    And the debt position gets status "VALID"

  # create VALID
  # pay Notice Number PAID
  # report REPORTED
  # get RETRIEVE
  Scenario: Pay a Debt Position Installment, from VALID to REPORTED
    Given a random iupd
    When the debt position is created using "v3" API
    Then the debt position gets the status code 201
    # Debt Position Publication
    When the debt position is published using "v3" API
    Then the debt position gets the status code 200 
    And the organization gets the nav value from "installment" after publication
    # Pay Payment Option
    When the payment option is paid
    Then the payment option gets the status code 200
    # Reporting the Transfer
    When the transfer is reported
    Then the transfer gets the status code 200