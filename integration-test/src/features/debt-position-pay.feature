Feature: Paying a debt position

 Background:
   Given GPD running

 Scenario: Pay Debt Position
   Given a random iupd
   When the debt position is created
   Then the debt position gets the status code 201
   # Debt Position Publication
   When the debt position is published
   Then the debt position gets the status code 200
   And the organization gets the nav value after publication
   # Pay Payment Option
   When the payment option is paid
   Then the payment option gets the status code 200
   # Reporting the Transfer
   When the transfer is reported
   Then the transfer gets the status code 200

 Scenario: Pay Debt Position with a validity date near
   Given a random iupd
   When the debt position with publish true and validity date in 10 seconds is created
   Then the debt position gets the status code 201
   # Pay Payment Option
   When system wait 10 seconds
   And the organization gets the nav value after creation
   And the payment option is paid
   Then the payment option gets the status code 200
   # Reporting the Transfer
   When the transfer is reported
   Then the transfer gets the status code 200