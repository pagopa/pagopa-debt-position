Feature: Managing a debt position

  Background:
    Given GPD running

  Scenario: Debt position creation
    Given a random iupd
    When the debt position is created
    And the debt position is updated and published
    Then the debt position gets the status code 200
    And the debt position gets status "VALID"

  Scenario: Update IBAN on all Organization's Transfers of updatable debt position
    When the updateTransferIbanMassive is called with oldIban "testIban" and newIban "testIban"
    Then the updateTransferIbanMassive gets the status code 200
    And the updateTransferIbanMassive response string includes "Updated IBAN on"