update apd.apd.transfer 
set iban = replace(iban, ' ', '')
where length (iban) > 27;