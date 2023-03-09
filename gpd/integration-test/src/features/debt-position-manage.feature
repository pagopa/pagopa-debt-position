Feature: Managing a debt position

  Background:
    Given GPD running

  Scenario: Debt position creation
    Given a random organization id and iupd
    When the debt position is created
    Then the debt position gets the status code 201

  Scenario: Debt position list
    When we ask the list of organizations debt positions
    Then we get the status code 200

  Scenario: Debt position update
  	When the debt position is updated
  	Then the organization gets the status code 200

  Scenario: Debt position get
  	When we get the debt position
    Then the company name is "Testing S.p.A."

  Scenario: Debt position published
    When the debt position is published
    Then the debt position gets the status code 200


