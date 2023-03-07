Feature: All about Organizations

  Background:
    Given GPD running

  Scenario: Debt position creation
    Given a random organization id
    When the debt position is created
    Then the debt position gets the status code 201

  Scenario: Debt position list
    When we ask the list of organizations debt positions
    Then we get the status code 200

  Scenario: Debt position update
  	Given a IUPD
  	When the debt position is updated
  	Then the organization gets the status code 200

  Scenario: Debt position get
  	Given a IUPD
  	When we get the debt position
    Then we get the status code 200

  Scenario: Debt position delete
    Given a IUPD
    When we delete the debt position
    Then we get the status code 200


