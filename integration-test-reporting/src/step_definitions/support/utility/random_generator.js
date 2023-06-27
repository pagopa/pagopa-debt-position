const { 
    CITIES_SET,
    COMPANY_SET,
    FIRST_NAME_SET,
    LAST_NAME_SET,
    STREET_SET
} = require("./constants")


function generateFirstName() {
    const idx = Math.floor(Math.random() * FIRST_NAME_SET.length);
    return FIRST_NAME_SET[idx]; 
}

function generateLastName() {
    const idx = Math.floor(Math.random() * LAST_NAME_SET.length);
    return LAST_NAME_SET[idx]; 
}

function generateCity() {
    const idx = Math.floor(Math.random() * CITIES_SET.length);
    return CITIES_SET[idx];
}

function generateStreet() {
    const idx = Math.floor(Math.random() * STREET_SET.length);
    return STREET_SET[idx];
}

function generateCompany() {
    const idx = Math.floor(Math.random() * COMPANY_SET.length);
    return COMPANY_SET[idx];
}

function generateRandomCharacter() {
    const characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
    return characters.charAt(Math.floor(Math.random() * characters.length));
}

function generateRandomNumber(numberOfDigits) {
    let result = "";
    const numbers = '0123456789';
    for ( var i = 0; i < numberOfDigits; i++ ) {
      result += numbers.charAt(Math.floor(Math.random() * numbers.length));
    }
    return result;
}

function generateFiscalCode(firstName, lastName) {
    const birthday = generateRandomDate(new Date(1940, 0, 1), new Date(2000, 0, 1));
    const cuttedFirstName = `${firstName.toUpperCase().replace(/[AEIOU]/g, "")}${firstName}${firstName.toUpperCase().replace(/[BCDFGHJKLMNPQRSTVWXYZ]/g, "")}`;
    const cuttedLastName = `${lastName.toUpperCase().replace(/[AEIOU]/g, "")}${lastName}${firstName.toUpperCase().replace(/[BCDFGHJKLMNPQRSTVWXYZ]/g, "")}`;
    const year = `${birthday.getFullYear()}`.substring(2, 4);
    const day = `${birthday.getDate() < 10 ? "0".concat(birthday.getDate()) : birthday.getDate()}`;
    const firstChar = generateRandomCharacter().toUpperCase();
    const secondChar = generateRandomCharacter().toUpperCase();
    const thirdChar = generateRandomCharacter().toUpperCase();
    const number = generateRandomNumber(3);
    return `${cuttedLastName.substring(0, 3)}${cuttedFirstName.substring(0, 3)}${year}${firstChar}${day}${secondChar}${number}${thirdChar}`;
}

function generateRandomDate(from, to) {
    return new Date(from.getTime() + Math.random() * (to.getTime() - from.getTime()));
}

module.exports = {
    generateCity,
    generateCompany,
    generateFirstName,
    generateFiscalCode,
    generateLastName,
    generateRandomDate,
    generateRandomNumber,
    generateStreet
}