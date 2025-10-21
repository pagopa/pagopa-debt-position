Feature: Retrieve a debt position

  Background:
    Given GPD running

  Scenario: Get Debt Position by IUV
    # Remove dirty data -> create Payment Positions -> Get Payment Position by IUV
    Given the debt position with IUPD 77777777777_GET_PP_BY_IUV is deleted
    And the debt position with IUPD 77777777777_GET_PP_BY_IUV and payment option with IUV 99000000000000000 is created
    And the response returns the status code 201
    When we get the debt position by IUV 99000000000000000
    Then the response returns the status code 200
    And the debt position response IUV value is 99000000000000000
    And the debt position with IUPD 77777777777_GET_PP_BY_IUV is deleted