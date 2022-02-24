package it.gov.pagopa.commons.annotation.validation.implement;

import java.util.regex.Matcher;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

import it.gov.pagopa.commons.annotation.validation.IvaCode;

public class IvaCodeValidator implements ConstraintValidator<IvaCode, String> {

    private java.util.regex.Pattern pattern;
    private String regex = "^[0-9]{11}$";

    @Override
    public boolean isValid(String value, ConstraintValidatorContext arg1) {
	if (value == null || value.length() != 11) {
	    return false;
	}
	Matcher m = pattern.matcher(value);
	if (!m.matches()) {
	    return false;
	}

	int i = 0;
	int c = 0;
	int s = 0;
	for (i = 0; i <= 9; i += 2) {
	    s += value.charAt(i) - '0';
	}
	for (i = 1; i <= 9; i += 2) {
	    c = 2 * (value.charAt(i) - '0');
	    if (c > 9) {
		c = c - 9;
	    }
	    s += c;
	}
	return (10 - s % 10) % 10 == value.charAt(10) - '0';
    }

    @Override
    public void initialize(IvaCode arg0) {
	pattern = java.util.regex.Pattern.compile(regex);
    }

}
