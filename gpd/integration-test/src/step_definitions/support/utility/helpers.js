function addDays(days) {
  var date = new Date();
  date.setDate(date.getDate() + days);
  return date;
}

function buildStringFromDate(rawDate) {
  var mm = rawDate.getMonth() + 1;
  var dd = rawDate.getDate();
  return [rawDate.getFullYear(), (mm>9 ? '' : '0') + mm, (dd>9 ? '' : '0') + dd].join('-');
}

function getValidBundle(gpdSessionBundle, gpsSessionBundle) {
  let bundle = undefined;
  if (gpsSessionBundle.isExecuting) {
    bundle = gpsSessionBundle;
  } else if (gpdSessionBundle.isExecuting) {
    bundle = gpdSessionBundle;
  }
  return bundle;
}

function makeidMix(length) {
  var result           = '';
  var characters       = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  var charactersLength = characters.length;
  for ( var i = 0; i < length; i++ ) {
    result += characters.charAt(Math.floor(Math.random() * charactersLength));
  }
  return result;
}

function makeidNumber(length) {
  var result           = '';
  var characters       = '0123456789';
  var charactersLength = characters.length;
  for ( var i = 0; i < length; i++ ) {
    result += characters.charAt(Math.floor(Math.random() * charactersLength));
  }
 return result;
}

module.exports = {
  addDays,
  buildStringFromDate,
  getValidBundle,
  makeidMix,
  makeidNumber,
}