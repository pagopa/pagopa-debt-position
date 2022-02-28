package it.gov.pagopa.payments.mock;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Objects;

public class MockUtil {

    public static <T> T readModelFromFile(String relativePath, Class<T> clazz) throws IOException {
        ClassLoader classLoader = MockUtil.class.getClassLoader();
        File file = new File(Objects.requireNonNull(classLoader.getResource(relativePath)).getPath());
        var content = Files.readString(file.toPath());
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        return objectMapper.readValue(content, clazz);
    }
}
