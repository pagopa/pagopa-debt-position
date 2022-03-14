package it.gov.pagopa.reporting.functions;

import com.microsoft.azure.functions.ExecutionContext;
import it.gov.pagopa.reporting.service.OptionsService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.logging.Logger;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class ElaborateOptionsTest {

    @Mock
    ExecutionContext context;

    @Spy
    ElaborateOptions function;

    @Mock
    OptionsService optionsService;

    private String readFromInputStream(InputStream inputStream) throws IOException {
        StringBuilder resultStringBuilder = new StringBuilder();
        try (BufferedReader br = new BufferedReader(new InputStreamReader(inputStream))) {
            String line;
            while ((line = br.readLine()) != null) {
                resultStringBuilder.append(line).append("\n");
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return resultStringBuilder.toString();
    }

    @Test
    void runOkTest() throws IOException {

        when(context.getLogger()).thenReturn(Logger.getLogger("InfoLogging"));

        ClassLoader classLoader = getClass().getClassLoader();
        InputStream inputStream = classLoader.getResourceAsStream("dataflow##idPA##idflow.xml");
        String data = readFromInputStream(inputStream);

        byte[] file = data.getBytes();

        Logger logger = Logger.getLogger("InfoLogging");
        doReturn(optionsService).when(function).getOptionsServiceInstance(logger);

        function.run(file, "dataflow##idPA##idflow.xml", context);

        verify(context, times(1)).getLogger();
        verify(optionsService, times(1)).optionsProcessing(any(), anyString(), anyString(), anyString());

    }

    @Test
    void getOptionsServiceIstanceTest() throws Exception {

        Logger logger = Logger.getLogger("testlogging");

        // test
        OptionsService istance = function.getOptionsServiceInstance(logger);

        assertNotNull(istance);
    }
}
