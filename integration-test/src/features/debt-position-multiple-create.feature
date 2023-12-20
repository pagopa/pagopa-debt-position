Feature: Managing a debt position

  Background:
    Given GPD running

  Scenario: Multiple Debt position creation
    Given a random iupd
    When the debit position items is created
    Then the debt position gets the status code 201
    
  #Scenario: Debt action action with segregation codes check
  #  Given a random iupd
  #  When the debt position using segregation codes is created
  #  Then the debt position gets the status code 201
  #  And the debt position gets status "DRAFT"
  #  When the debt position using segregation codes is published
  #  Then the debt position gets the status code 200
  #  And the debt position gets status "VALID"
  #  When the debt position using segregation codes is invalidated
  #  Then the debt position gets the status code 200
  #  And the debt position gets status "INVALID"