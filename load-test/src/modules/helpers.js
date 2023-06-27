export function randomString(length, charset) {
  let res = '';
  while (length--) res += charset[(Math.random() * charset.length) | 0];
  return res;
}

export function generateFakeFiscalCode(decade) {
  const s = randomString(6, "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
  const d = randomString(7, "0123456789")
  return [s, decade, d[1], "A", d[2], d[3], "Y", d[4], d[5], d[6], "X"].join(
    ""
  );
}

export function getDateNowISOAddMin(minutesToAdd) {
  const currentDate = new Date();
  const date = new Date(currentDate.getTime() + minutesToAdd*60000);
  return date.toISOString();    
}

export function getDateNowISOSubMin(minutesToSub) {
  const currentDate = new Date();
  const date = new Date(currentDate.getTime() - minutesToSub*60000);
  return date.toISOString();    
}

export function makeidMix(length) {
  var result           = '';
  var characters       = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  var charactersLength = characters.length;
  for ( var i = 0; i < length; i++ ) {
    result += characters.charAt(Math.floor(Math.random() * charactersLength));
  }
  return result;
}

export function makeidNumber(length) {
  var result           = '';
  var characters       = '0123456789';
  var charactersLength = characters.length;
  for ( var i = 0; i < length; i++ ) {
    result += characters.charAt(Math.floor(Math.random() * 
charactersLength));
 }
 return result;
}

Date.prototype.addDays = function(days) {
  var date = new Date(this.valueOf());
  date.setDate(date.getDate() + days);
  return date;
}
Date.prototype.subDays = function(days) {
  var date = new Date(this.valueOf());
  date.setDate(date.getDate() - days);
  return date;
}

export function getRandomItemFromArray(items) {
	return items[Math.floor(Math.random()*items.length)];
}

export function getPayload(iupd, iuv, due_date, retention_date, transfer_id) {
    return JSON.stringify(
       {
         "iupd": iupd,
         "type": "F",
         "fiscalCode": "JHNDOE00A01F205N",
         "fullName": "John Doe",
         "streetName": "streetName",
         "civicNumber": "11",
         "postalCode": "00100",
         "city": "city",
         "province": "RM",
         "region": "RM",
         "country": "IT",
         "email": "lorem@lorem.com",
         "phone": "333-123456789",
         "companyName": "companyName",
         "officeName": "officeName",
         "paymentOption": [
           {
             "iuv": iuv,
             "amount": 10000,
             "description": "Canone Unico Patrimoniale - CORPORATE",
             "isPartialPayment": false,
             "dueDate": due_date,
             "retentionDate": retention_date,
             "fee": 0,
             "transfer": [
               {
                 "idTransfer": transfer_id,
                 "amount": 10000,
                 "remittanceInformation": "remittanceInformation 1",
                 "category": "9/0101108TS/",
                 "iban": "IT0000000000000000000000000"
               }
             ]
           }
         ]
       }
     );
}
