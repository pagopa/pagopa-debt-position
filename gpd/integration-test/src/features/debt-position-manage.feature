Feature: All about Organizations

  Background:
    Given GPD running

  Scenario: Debt position creation
    Given a random organization id
    When the debt position is created
    Then the debt position gets the status code 201


