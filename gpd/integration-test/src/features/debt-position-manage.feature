Feature: Managing a debt position

  Background:
    Given GPD running

  Scenario: Debt position creation
    Given a random organization id and iupd
    When the debt position is created
    Then the debt position gets the status code 201

  Scenario: Debt position filter list by status and due date
    Given the filter made by status "DRAFT"
    And the filter made by due date from today to 10 days
    When we ask the list of organizations debt positions
    Then we get the status code 200

  Scenario: Debt position filter list by payment date
    Given the filter made by payment date from today to 20 days
    When we ask the list of organizations debt positions
    Then we get the status code 200

  Scenario: Debt position update
  	When the debt position is updated
  	Then the organization gets the status code 200

  Scenario: Debt position get
  	When we get the debt position
    Then the company name is "Testing S.p.A."

  Scenario: Debt position deleted
    When the debt position is deleted
    Then the debt position gets the status code 200

  Scenario: Debt position filter list: exclusive param violation
    Given the filter made by due date from today to 10 days
    And the filter made by payment date from today to 20 days
    When we ask the list of organizations debt positions
    Then we get the status code 400