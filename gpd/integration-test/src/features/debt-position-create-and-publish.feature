Feature: Managing a debt position

  Background:
    Given GPD running

  Scenario: Debt position creation
    Given a random organization id and iupd
    When the debt position is created and published
    Then the debt position gets the status code 201
    And the debt position gets status "VALID"


