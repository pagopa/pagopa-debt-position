Feature: Massive debt positions creation

  Background:
    Given GPD running

  Scenario: Multiple Debt positions creation
    Given a random iupd
    When the debt position items is created
    Then the debt position gets the status code 201

  Scenario: Multiple Debt actions with segregation codes check
     Given a random iupd
     When the debt position items, using segregation codes, is created
     Then the debt position gets the status code 201