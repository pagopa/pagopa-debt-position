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

export function generateFakeStringArray(stringLength, numOfArrayElement, charset) {
	let res = [];
	for (let i = 0; i < numOfArrayElement; i++) {
		res[i] = randomString(stringLength, charset);
	}
	return res;
}


