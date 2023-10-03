package it.gov.pagopa.debtposition.util;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;

import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;

public class LocalDateTimeDeserializer extends StdDeserializer<LocalDateTime>{

	/**
	 * generated serialVersionUID
	 */
	private static final long serialVersionUID = -7714561619990964785L;

	protected LocalDateTimeDeserializer() {
		super(LocalDateTime.class);
	}

	@Override
	public LocalDateTime deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
		String value = p.readValueAs(String.class);
        try {	
        	DateTimeFormatter formatter = DateTimeFormatter.ofPattern("[yyyy-MM-dd'T'HH:mm:ss.SSS'Z'][yyyy-MM-dd'T'HH:mm:ss]");
        	return LocalDateTime.parse(value, formatter);
        } catch (DateTimeParseException e) {
        	throw new AppException(AppError.DEBT_POSITION_REQUEST_DATA_ERROR, value + " is not a valid date format ");
        }
	}

}
