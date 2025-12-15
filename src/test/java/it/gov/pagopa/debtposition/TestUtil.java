package it.gov.pagopa.debtposition;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Objects;
import lombok.experimental.UtilityClass;

@UtilityClass
public class TestUtil {

  // --- METODI ESISTENTI, lasciati invariati ---

  public String readJsonFromFile(String relativePath) throws IOException {
    ClassLoader classLoader = TestUtil.class.getClassLoader();
    File file = new File(Objects.requireNonNull(classLoader.getResource(relativePath)).getPath());
    return Files.readString(file.toPath());
  }

  public <T> T readObjectFromFile(String relativePath, Class<T> clazz) throws IOException {
    ClassLoader classLoader = TestUtil.class.getClassLoader();
    File file = new File(Objects.requireNonNull(classLoader.getResource(relativePath)).getPath());
    return new ObjectMapper().readValue(file, clazz);
  }

  public File readFile(String relativePath) {
    ClassLoader classLoader = TestUtil.class.getClassLoader();
    return new File(Objects.requireNonNull(classLoader.getResource(relativePath)).getFile());
  }

  public String toJson(Object object) throws JsonProcessingException {
    return new ObjectMapper()
        .registerModule(new JavaTimeModule())
        .configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false)
        .writeValueAsString(object);
  }

  // --- Overload with ObjectMapper parameter ---

  // Uses the passed mapper for serialization.
  public String toJson(Object object, ObjectMapper objectMapper) throws JsonProcessingException {
    return (objectMapper != null ? objectMapper : new ObjectMapper()
            .registerModule(new JavaTimeModule())
            .configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false))
        .writeValueAsString(object);
  }

  // Uses the passed mapper for deserialization.
  public <T> T readObjectFromFile(String relativePath, Class<T> clazz, ObjectMapper objectMapper)
		  throws IOException {
	  ObjectMapper defaultOM =
			  new ObjectMapper()
			  .registerModule(new JavaTimeModule())
			  .configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
	  ClassLoader classLoader = TestUtil.class.getClassLoader();
	  File file = new File(Objects.requireNonNull(classLoader.getResource(relativePath)).getPath());
	  ObjectMapper om = (objectMapper != null ? objectMapper : defaultOM);
	  return om.readValue(file, clazz);
  }
}
