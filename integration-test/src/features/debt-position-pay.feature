Feature: Paying a debt position

 Background:
   Given GPD running

 Scenario: Debt position creation
   Given a random iupd
   When the debt position is created
   Then the debt position gets the status code 201
   # Debt position publication
   When the debt position is published
   Then the debt position gets the status code 200
   And the organization gets the nav value after publication
   # Payment option payment
   When the payment option is paid
   Then the payment option gets the status code 200
   # Reporting the transfer
   When the transfer is reported
   Then the transfer gets the status code 200