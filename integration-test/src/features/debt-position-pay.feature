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

 Scenario: Debt Position Payability with a near validity date
   # Create Debt Position
   Given a random iupd
   When the debt position with validityDate in 5 seconds is created
   Then the debt position gets the status code 201
   # Check Payability
   When system wait 10 seconds
   And we get the payment option by iuv
   Then the debt position is in the status "VALID"
   # Delete Debt position
   When the debt position is deleted
   Then the debt position gets the status code 200