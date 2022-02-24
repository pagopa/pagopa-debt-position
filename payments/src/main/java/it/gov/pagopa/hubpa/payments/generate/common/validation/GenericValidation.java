package it.gov.pagopa.hubpa.payments.generate.common.validation;

import java.util.Calendar;
import java.util.Collections;
import java.util.Map;
import java.util.TreeMap;

import it.gov.pagopa.hubpa.payments.iuvgenerator.common.ErrorMessages;

/**
 * Class with common validation methods
 */
public class GenericValidation {
    private GenericValidation() {
	throw new IllegalStateException("GenericValidation class");
    }

    private static TreeMap<Character, Integer> map = new TreeMap<>();

    private static Map<Character, Integer> DIGITCONVERSIONMAP;

    /**
     * Fiscal code validation
     * 
     * @param cf fiscal code
     * @return 0 if success<br/>
     *         != 0 if error
     */
    public static int checkFiscalCode(String cf) {

	map.put('L', 0);
	map.put('M', 1);
	map.put('N', 2);
	map.put('P', 3);
	map.put('Q', 4);
	map.put('R', 5);
	map.put('S', 6);
	map.put('T', 7);
	map.put('U', 8);
	map.put('V', 9);

	DIGITCONVERSIONMAP = Collections.unmodifiableSortedMap(map);

	Calendar calendar = Calendar.getInstance();

	cf = deleteWhiteSpace(cf);
	if (cf != null) {
	    cf = cf.toUpperCase();
	}

	if (cf == null || cf.length() != 16) {
	    return 1;
	}

	for (int i = 0; i < 6; i++) {
	    if (!(Character.isLetter(cf.charAt(i)))) {
		return 2;
	    }
	}

	try {
	    calendar.set(Calendar.YEAR, getYear(cf.substring(6, 8)));
	} catch (Exception e) {
	    return 3;
	}
	try {
	    Integer month = getMonth(cf.charAt(8));
	    if (month != null) {
		calendar.set(Calendar.MONTH, month);
	    } else {
		return 4;
	    }
	} catch (Exception e) {
	    return 4;
	}

	try {
	    String dd = cf.substring(9, 11);
	    int day = Integer.parseInt(getDigitPerOmonimia(dd.charAt(0)) + "" + getDigitPerOmonimia(dd.charAt(1)));
	    if (day > 40) {
		day -= 40;
	    }

	    calendar.set(Calendar.DAY_OF_MONTH, day);
	} catch (Exception e) {
	    return 5;
	}
	return checkCharFrom11To15(cf);
	
    }
    private static int checkCharFrom11To15(String cf) {
	if (!(Character.isLetter(cf.charAt(11)))) {
	    return 6;
	}

	if (!(Character.isDigit(cf.charAt(12)))) {
	    return 6;
	}

	if (!(Character.isDigit(cf.charAt(13)))) {
	    return 6;
	}

	if (!(Character.isDigit(cf.charAt(14)))) {
	    return 6;
	}

	if (!(Character.isLetter(cf.charAt(15)))) {
	    return 7;
	}
	return 0;
    }
    private static String deleteWhiteSpace(String str) {
	if (str == null || str.length() == 0) {
	    return str;
	}
	int sz = str.length();
	char[] chs = new char[sz];
	int count = 0;
	for (int i = 0; i < sz; i++) {
	    if (!Character.isWhitespace(str.charAt(i))) {
		chs[count++] = str.charAt(i);
	    }
	}
	if (count == sz) {
	    return str;
	}
	return new String(chs, 0, count);
    }

    private static int getYear(String yy) {
	if (yy == null || yy.length() != 2) {
	    throw new IllegalArgumentException(ErrorMessages.VALIDATION_FC_YEAR_ERROR + yy);
	} else {
	    yy = getDigitPerOmonimia(yy.charAt(0)) + "" + getDigitPerOmonimia(yy.charAt(1));

	    Calendar c = Calendar.getInstance();
	    int yyyy = c.get(Calendar.YEAR);

	    String yySup = String.valueOf(yyyy).substring(0, 2);
	    String yyInf = String.valueOf(yyyy).substring(2, 4);

	    yyyy = Integer.parseInt(yyInf);
	    if (yyyy > Integer.parseInt(yy)) {
		yyyy = Integer.parseInt(yySup + yy);
	    } else {
		yyyy = Integer.parseInt(yySup + "00") - 100 + Integer.parseInt(yy);
	    }
	    return yyyy;
	}

    }

    private static Integer getMonth(char character) {
	Integer mm = null;
	switch (character) {
	case 'A':
	    mm = Calendar.JANUARY;
	    break;
	case 'B':
	    mm = Calendar.FEBRUARY;
	    break;
	case 'C':
	    mm = Calendar.MARCH;
	    break;
	case 'D':
	    mm = Calendar.APRIL;
	    break;
	case 'E':
	    mm = Calendar.MAY;
	    break;
	case 'H':
	    mm = Calendar.JUNE;
	    break;
	case 'L':
	    mm = Calendar.JULY;
	    break;
	case 'M':
	    mm = Calendar.AUGUST;
	    break;
	case 'P':
	    mm = Calendar.SEPTEMBER;
	    break;
	case 'R':
	    mm = Calendar.OCTOBER;
	    break;
	case 'S':
	    mm = Calendar.NOVEMBER;
	    break;
	case 'T':
	    mm = Calendar.DECEMBER;
	    break;
	default:
	    break;
	}
	return mm;
    }

    private static int getDigitPerOmonimia(char character) {
	if (Character.isDigit(character)) {
	    return Integer.parseInt(String.valueOf(character));
	} else {
	    Integer result = DIGITCONVERSIONMAP.get(Character.toUpperCase(character));
	    if (result == null) {
		throw new IllegalArgumentException(ErrorMessages.VALIDATION_FC_DIGIT_ERROR + character);
	    }
	    return result;
	}
    }

    /**
     * VAT number validation
     * 
     * @param pi VAT number
     * @return true if success
     */
    public static boolean checkVatNumber(String pi) {
	int i;
	int c;
	int s;
	if (pi.length() == 0)
	    return false;
	if (pi.length() != 11)
	    return false;
	for (i = 0; i < 11; i++) {
	    if (pi.charAt(i) < '0' || pi.charAt(i) > '9') {
		return false;
	    }
	}
	s = 0;
	for (i = 0; i <= 9; i += 2)
	    s += pi.charAt(i) - '0';
	for (i = 1; i <= 9; i += 2) {
	    c = 2 * (pi.charAt(i) - '0');
	    if (c > 9) {
		c = c - 9;
	    }
	    s += c;
	}
	if ((10 - s % 10) % 10 != pi.charAt(10) - '0')
	    return false;
	return true;
    }
}
