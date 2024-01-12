Feature: Managing a debt position

  Background: 
    Given GPD running

  Scenario: Debt position creation
    Given a random iupd
    When the debt position is created
    Then the debt position gets the status code 201
    And the organization gets the nav value after creation

  Scenario: Debt position notification fee update
    When the notification fee of the debt position is updated
    Then the organization gets the status code 209
    And the organization gets the updated amounts

  Scenario: Debt position filter list by status and due date
    Given the filter made by status "VALID"
    And the filter made by due date from today to 10 days
    When we ask the list of organizations debt positions
    Then we get the status code 200

  Scenario: Debt position filter list by payment date
    Given the filter made by payment date from today to 20 days
    When we ask the list of organizations debt positions
    Then we get the status code 200

  Scenario: Debt position update
    When the debt position is updated
    Then the organization gets the update status code 200
    And the organization gets the nav value after update

  Scenario: Debt position get
    When we get the debt position
    Then the company name is "Testing S.p.A."
    And the organization get the nav value

  Scenario: Debt position deleted
    When the debt position is deleted
    Then the debt position gets the status code 200

  Scenario: Debt position filter list: exclusive param violation
    Given the filter made by due date from today to 10 days
    And the filter made by payment date from today to 20 days
    When we ask the list of organizations debt positions
    Then we get the status code 400

  Scenario: Debt position notification fee update by querying the node with existing positions
    Given a random iupd
    When a node OK result debt position is created
    Then the debt position gets the status code 201
    And the organization gets the nav value after creation
    When a node KO result debt position is created
    Then the debt position gets the status code 201
    And the organization gets the nav value after creation
    When the notification fee of the debt position is updated using an OK position on the node
    Then the organization gets the status code 200
    And the organization gets the updated amounts
    And the organization gets the updated last updated date notification fee
    When the notification fee of the debt position is updated using an KO position on the node
    Then the organization gets the status code 209
    And the organization gets the updated amounts
    And the organization gets the updated last updated date notification fee
    
  Scenario: Debt position manage with segregation codes check
    Given a random iupd
    When the debt position using segregation codes is created
    Then the debt position gets the status code 201
    And the organization gets the nav value after creation
    When the debt position using segregation codes is updated
    Then the organization gets the update status code 200
    And the organization gets the nav value after update
    When the organization gets the debt position using segregation codes
    Then the company name is "Testing S.p.A."
    And the organization get the nav value
    When the organization gets the list of debt positions using segregation codes
    Then we get the status code 200
    And the debt positions list size is greater than 1
    When the debt position using segregation codes is deleted
    Then the debt position gets the status code 200
