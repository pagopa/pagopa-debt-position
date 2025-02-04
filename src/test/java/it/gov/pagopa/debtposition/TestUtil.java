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

  /**
   * @param relativePath Path from source root of the json file
   * @return the Json string read from the file
   * @throws IOException if an I/O error occurs reading from the file or a malformed or unmappable
   *     byte sequence is read
   */
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

  /**
   * @param relativePath Path from source root of the file
   * @return the requested file
   */
  public File readFile(String relativePath) {
    ClassLoader classLoader = TestUtil.class.getClassLoader();
    return new File(Objects.requireNonNull(classLoader.getResource(relativePath)).getFile());
  }

  /**
   * @param object to map into the Json string
   * @return object as Json string
   * @throws JsonProcessingException if there is an error during the parsing of the object
   */
  public String toJson(Object object) throws JsonProcessingException {
    return new ObjectMapper()
        .registerModule(new JavaTimeModule())
        .configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false)
        .writeValueAsString(object);
  }
}
