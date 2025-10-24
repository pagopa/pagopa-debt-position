Feature: CRUD operations on debt position using v3 API

  Background:
    Given GPD running

  # create a debt position
  # get the debt position filtering by status
  # update the debt position
  # delete the debt position
  Scenario: Debt position CRUD happy case
    Given a random iupd
    When the debt position is created using "v3" API
    Then the debt position gets the status code 201
    And the organization gets the nav value from "installment" after creation
    # Debt position filter list by status and due date
    Given the filter made by status "DRAFT"
    And the filter made by due date from today to 10 days
    When we ask the list of organizations debt positions using "v3" API
    Then we get the status code 200
    # Debt position update
    When the debt position is updated using "v3" API
    Then the organization gets the update status code 200
    And the organization gets the nav value from "installment" after update
    # Debt position get
    When we get the debt position using "v3" API
    Then the company name is "Testing S.p.A."
    And the organization get the nav value from "installment"
    # Debt position deleted
    When the debt position is deleted using "v3" API
    Then the debt position gets the status code 200