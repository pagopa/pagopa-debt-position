Feature: Paying a debt position

  Background:
    Given GPD running

  Scenario: Debt position creation
    Given a random organization id and iupd
    When the debt position is created
    Then the debt position gets the status code 201

  Scenario: Debt position publication
    When the debt position is published
    Then the debt position gets the status code 200

  Scenario: Payment option publication
    When the payment option is paid
    Then the payment option gets the status code 200

  Scenario: Reporting the transfer
    When the transfer is reported
    Then the transfer gets the status code 200



