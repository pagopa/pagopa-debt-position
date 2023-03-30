Feature: All about flow report retrieving workflow - Complete flow

  Background:
    Given GPD service running
    * APIConfig service running
    * GPD Payments service running
    * reporting analysis service running
    * a paid debt position
    * a report flow sent to Node 

  Scenario: Reporting - Complete Happy path
    When the reporting batch analyzes the reporting flows for the organization
    And the client waits its execution
    Then the client asks the flow list for the organization
    * the client receives status code 200
    * the client receives a non-empty list of flows
    And the client asks the detail for one of the report flows
    * the client receives status code 200
    * the client receives the flow XML content
    And the client asks the detail for the analyzed debt positions
    * the client receives status code 200
    * the client receives the payment options with status "REPORTED"

