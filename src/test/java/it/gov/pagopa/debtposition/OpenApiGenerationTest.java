package it.gov.pagopa.debtposition;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

@SpringBootTest
@AutoConfigureMockMvc
class OpenApiGenerationTest {

  @Autowired ObjectMapper objectMapper;

  @Autowired private MockMvc mvc;

  @Test
  void swaggerSpringPlugin() throws Exception {
    saveOpenAPI("/v3/api-docs/internal_v1", "openapi_internal_v1.json");
    saveOpenAPI("/v3/api-docs/internal_v2", "openapi_internal_v2.json");
    saveOpenAPI("/v3/api-docs/external_v1", "openapi_external_v1.json");
    saveOpenAPI("/v3/api-docs/external_v2", "openapi_external_v2.json");
    saveOpenAPI("/v3/api-docs/external_v3", "openapi_external_v3.json");
    saveOpenAPI("/v3/api-docs/send_v1", "openapi_send_v1.json");
    saveOpenAPI("/v3/api-docs/aca_v1", "openapi_aca_v1.json");
  }

  private void saveOpenAPI(String fromUri, String toFile) throws Exception {
    mvc.perform(MockMvcRequestBuilders.get(fromUri).accept(MediaType.APPLICATION_JSON))
            .andExpect(MockMvcResultMatchers.status().is2xxSuccessful())
            .andDo(
                    (result) -> {
                      assertNotNull(result);
                      assertNotNull(result.getResponse());
                      final String content = result.getResponse().getContentAsString();
                      assertFalse(content.isBlank());
//                      assertFalse(content.contains("${"), "Generated swagger contains placeholders");
                      Object swagger =
                              objectMapper.readValue(result.getResponse().getContentAsString(), Object.class);

                      String formatted = objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(swagger);
                      Path basePath = Paths.get("openapi/");
                      Files.createDirectories(basePath);
                      Files.write(basePath.resolve(toFile), formatted.getBytes());
                    });
  }

}
