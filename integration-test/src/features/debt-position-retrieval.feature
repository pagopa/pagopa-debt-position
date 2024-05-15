Feature: Retrieve a debt position

  Background: 
    Given GPD running

  Scenario: Get Debt Position by IUV
    Given the debt position with IUPD "77777777777" and payment option with IUV "99000000000000000" is created
    When we get the debt position by IUV "99000000000000000"
    Then the debt position response IUV value is "99000000000000000"
