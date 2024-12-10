package it.gov.pagopa.debtposition;

import com.fasterxml.jackson.databind.ObjectMapper;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

@SpringBootTest
@AutoConfigureMockMvc
class OpenApiGenerationTest {

    @Autowired
    ObjectMapper objectMapper;

    @Autowired
    private MockMvc mvc;

    @Test
    void swaggerSpringPlugin() throws Exception {
    	// Each block of {} represents a json path. For example 'paths/.../post/paramenters'
    	String[][] pathList = {{"paths","/organizations/{organizationfiscalcode}/debtpositions","post","parameters"}}; 
        saveOpenAPI("/v3/api-docs", "openapi_internal.json", new String[0][0], new String[0]);
        saveOpenAPI("/v3/api-docs/external", "openapi_external.json", pathList, "serviceType");
        saveOpenAPI("/v3/api-docs/external_odp", "openapi_external_odp.json",  pathList, "serviceType");
        saveOpenAPI("/v3/api-docs/send", "openapi_send.json", new String[0][0], new String[0]);
        saveOpenAPI("/v3/api-docs/external_massive", "openapi_external_massive.json", pathList, "serviceType");
        saveOpenApiInternalMassive("generate_openapi.sh");
    }

    private void saveOpenAPI(String fromUri, String toFile, String[][] pathList, String...namesToRemove  ) throws Exception {
        mvc.perform(MockMvcRequestBuilders.get(fromUri).accept(MediaType.APPLICATION_JSON))
                .andExpect(MockMvcResultMatchers.status().is2xxSuccessful())
                .andDo(
                        (result) -> {
                            assertNotNull(result);
                            assertNotNull(result.getResponse());
                            final String content = result.getResponse().getContentAsString();
                            assertFalse(content.isBlank());
                            assertFalse(content.contains("${"), "Generated swagger contains placeholders");
                            Object swagger =
                                    objectMapper.readValue(result.getResponse().getContentAsString(), Object.class);
                            
                            // removing unwanted fields
                            if (namesToRemove != null && pathList != null && pathList.length > 0 && namesToRemove.length > 0) {
                            	removeField(swagger, pathList, namesToRemove);
                            }
                            
                            String formatted =
                                    objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(swagger);
                            Path basePath = Paths.get("openapi/");
                            Files.createDirectories(basePath);
                            Files.write(basePath.resolve(toFile), formatted.getBytes());
                        });
    }
    
    private void removeField(Object obj, String[][] pathList, String[] namesToRemove) throws Exception {
        for (String[] pathNames : pathList) {
            for (String nameToRemove : namesToRemove) {
            	// start iterating from the first element
                removeFieldRecursive(obj, pathNames, nameToRemove, 0);
            }
        }
    }
    
    private void removeFieldRecursive(Object obj, String[] pathNames, String nameToRemove, int index) throws Exception {
        if (obj == null || index >= pathNames.length) {
            return;
        }

        String fieldPathName = pathNames[index];

        if (obj instanceof Map) {
            // If the object is a map (JSON structure)
            @SuppressWarnings("unchecked")
            Map<String, Object> map = (Map<String, Object>) obj;

            // If is the last level, remove the field
            if (index == pathNames.length - 1) {
                Object param = map.get(fieldPathName);
                if (param instanceof List) {
                    List<?> list = (List<?>) param;
                    // remove the blocks that have the "name" field equal to nameToRemove (ex. serviceType)
                    list.removeIf(item -> {
                        if (item instanceof Map) {
                            @SuppressWarnings("unchecked")
							Map<String, Object> paramMap = (Map<String, Object>) item;
                            return nameToRemove.equals(paramMap.get("name"));
                        }
                        return false;
                    });
                }
            } else {
                Object nextObj = map.get(fieldPathName);
                removeFieldRecursive(nextObj, pathNames, nameToRemove, index + 1);
            }
        } else if (obj instanceof List) {
            // If the object is a list (JSON array), iterate over each element of the list
            @SuppressWarnings("unchecked")
            List<Object> list = (List<Object>) obj;
            for (Object item : list) {
                removeFieldRecursive(item, pathNames, nameToRemove, index);
            }
        }
    }
    
    private void saveOpenApiInternalMassive (String scriptFileName) throws IOException, InterruptedException {
        
        Path root = FileSystems.getDefault().getPath("").toAbsolutePath();
        String scriptPath = Paths.get(root.toString(),"openapi", scriptFileName).toString();
    	
    	// Detect the operating system
    	String os = System.getProperty("os.name").toLowerCase();
    	
    	String[] command = {"bash"};
    	
    	if (os.contains("win")) {
    		// WSL must be present on Windows OS
    		Pattern pattern = Pattern.compile("([a-zA-Z]):");
            Matcher matcher = pattern.matcher(scriptPath);
            String pathWSL = matcher.replaceFirst(m -> "/mnt/" + m.group(1).toLowerCase());
            pathWSL = pathWSL.replace("\\", "/");
            scriptPath = pathWSL;
            command = new String[]{"wsl", "dos2unix", "bash"};  
    	} 
    	
    	OpenApiGenerationTest.executeScript(os, scriptPath, command);
    }
    
    private static void executeScript(String os, String scriptPath, String...command) throws IOException, InterruptedException {

    	if (os.contains("win")) {
    		Process dos2unixProcess = Runtime.getRuntime().exec(new String[] {command[0],command[1],scriptPath});
    		int dos2unixExitCode = dos2unixProcess.waitFor();
    		assertEquals(0, dos2unixExitCode);
    		if (dos2unixExitCode == 0) {
    			Process bashProcess = Runtime.getRuntime().exec(new String[] {command[0],command[2],scriptPath});
    			int bashExitCode = bashProcess.waitFor();
    			assertEquals(0, bashExitCode);
    		}
    	} else {
    		Process bashProcess = Runtime.getRuntime().exec(new String[] {command[0],scriptPath});
    		int bashExitCode = bashProcess.waitFor();
    		assertEquals(0, bashExitCode);
    	}
    }
}
