package it.gov.pagopa.commons.annotation.validation.implement;

import java.util.regex.Matcher;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

import it.gov.pagopa.commons.annotation.validation.FiscalCode;

public class FiscalCodeValidator  implements ConstraintValidator<FiscalCode, String>{

	 private java.util.regex.Pattern pattern;
	 private String regex = "^[A-Za-z]{6}[0-9]{2}[A-Za-z][0-9]{2}[A-Za-z][0-9]{3}[A-Za-z]$";
	 private int[] dispari = { 1, 0, 5, 7, 9, 13, 15, 17, 19, 21, 2, 4, 18, 20,
	            11, 3, 6, 8, 12, 14, 16, 10, 22, 25, 24, 23 };
	
	@Override
	public void initialize(FiscalCode arg0) {
		 pattern = java.util.regex.Pattern.compile(regex);
		
	}

	@Override
	public boolean isValid(String value, ConstraintValidatorContext arg1) {
		if (value == null || value.length() == 0) {
            return true;
        }
 
        Matcher m = pattern.matcher(value);
 
        if (!m.matches()) {
            return false;
        }
 
        String cf = value.toUpperCase();
 
        int i = 0;
        int s = 0;
        int c = 0;
        int t = 0;

        for (i = 1; i <= 13; i += 2) {
            c = cf.charAt(i);
            if (c >= '0' && c <= '9') {
                s = s + c - '0';
            } else {
                s = s + c - 'A';
            }
        }
        for (i = 0; i <= 14; i += 2) {
            c = cf.charAt(i);
            if (c >= '0' && c <= '9') {
                c = c - '0'+'A';
            }
            c=c-'A';
            s = s + dispari[c];
            t =  t + dispari[c];
        }
 
        return s % 26 + 'A' == cf.charAt(15);
    }
}
