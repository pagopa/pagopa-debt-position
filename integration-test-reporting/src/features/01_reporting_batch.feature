# Feature: All about flow report retrieving workflow - Reporting batch component

#   Background:
#     Given GPD service running
#     * GPD Payments service running
#     * APIConfig service running
#     * reporting analysis service running
#     * a paid debt position

#   Scenario: Reporting - Reporting batch no computed flow
#     When the reporting batch analyzes the reporting flows for the organization
#     And the client waits its execution
#     Then the client asks the flow list for the organization
#     * the client receives status code 200
#     * the client receives an empty list of flows

#   Scenario: Reporting - Reporting batch Happy path
#     Given a report flow sent to Node 
#     When the reporting batch analyzes the reporting flows for the organization
#     And the client waits its execution
#     Then the client asks the flow list for the organization
#     * the client receives status code 200
#     * the client receives a non-empty list of flows