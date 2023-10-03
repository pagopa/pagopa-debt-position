package it.gov.pagopa.debtposition.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mock;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;

import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.ObjectMapper;

import it.gov.pagopa.debtposition.exception.AppException;

class LocalDateTimeDeserializerTest {
	
	private ObjectMapper mapper = new ObjectMapper();
    private LocalDateTimeDeserializer deserializer = new LocalDateTimeDeserializer();
	
	@Test
	void deserializeAcceptedDateFormat1_OK() throws JsonProcessingException, IOException {
		String json = String.format("{\"date\":%s}", "\"2023-10-03T13:07:30.787Z\"");
		InputStream stream = new ByteArrayInputStream(json.getBytes(StandardCharsets.UTF_8));
        JsonParser parser = mapper.getFactory().createParser(stream);
	    DeserializationContext ctxt = mock(DeserializationContext.class);
	    JsonToken token;
	    LocalDateTime value = null;
	    while ((token = parser.nextValue()) != null) {
            switch (token) {
                case VALUE_STRING:
                	value = deserializer.deserialize(parser, ctxt);
                    break;
                default:
                    break;
            }
        }
        assertNotNull(value);
	}
	
	@Test
	void deserializeAcceptedDateFormat2_OK() throws JsonProcessingException, IOException {
		String json = String.format("{\"date\":%s}", "\"2023-10-03T13:07:30\"");
		InputStream stream = new ByteArrayInputStream(json.getBytes(StandardCharsets.UTF_8));
        JsonParser parser = mapper.getFactory().createParser(stream);
	    DeserializationContext ctxt = mock(DeserializationContext.class);
	    JsonToken token;
	    LocalDateTime value = null;
	    while ((token = parser.nextValue()) != null) {
            switch (token) {
                case VALUE_STRING:
                	value = deserializer.deserialize(parser, ctxt);
                    break;
                default:
                    break;
            }
        }
        assertNotNull(value);
	}
	
	@Test
	void deserializeNotAcceptedDateFormat1_KO() throws JsonProcessingException, IOException {
		String json = String.format("{\"date\":%s}", "\"2023-10-03T13:07:30.787\"");
		InputStream stream = new ByteArrayInputStream(json.getBytes(StandardCharsets.UTF_8));
        JsonParser parser = mapper.getFactory().createParser(stream);
	    DeserializationContext ctxt = mock(DeserializationContext.class);
	    JsonToken token;
	    while ((token = parser.nextValue()) != null) {
            switch (token) {
                case VALUE_STRING:
                	try {
                		deserializer.deserialize(parser, ctxt);
                		fail();
                	} catch (AppException e) {
                		assertEquals(HttpStatus.BAD_REQUEST,e.getHttpStatus());
                		
                	} catch (Exception e) {
                		fail();
                	}
                    break;
                default:
                    break;
            }
        }
	}
	
	@Test
	void deserializeNotAcceptedDateFormat2_KO() throws JsonProcessingException, IOException {
		String json = String.format("{\"date\":%s}", "\"2023-10-03\"");
		InputStream stream = new ByteArrayInputStream(json.getBytes(StandardCharsets.UTF_8));
        JsonParser parser = mapper.getFactory().createParser(stream);
	    DeserializationContext ctxt = mock(DeserializationContext.class);
	    JsonToken token;
	    while ((token = parser.nextValue()) != null) {
            switch (token) {
                case VALUE_STRING:
                	try {
                		deserializer.deserialize(parser, ctxt);
                		fail();
                	} catch (AppException e) {
                		assertEquals(HttpStatus.BAD_REQUEST,e.getHttpStatus());
                		
                	} catch (Exception e) {
                		fail();
                	}
                    break;
                default:
                    break;
            }
        }
	}

}
