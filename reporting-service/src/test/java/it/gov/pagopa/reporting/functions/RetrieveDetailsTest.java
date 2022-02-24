package it.gov.pagopa.reporting.functions;

import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.logging.Logger;

import com.microsoft.azure.functions.ExecutionContext;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

import it.gov.pagopa.reporting.service.FlowsService;

@ExtendWith(MockitoExtension.class)
class RetrieveDetailsTest {

    @Mock
    ExecutionContext context;

    @Spy
    RetrieveDetails function;

    @Mock
    FlowsService flowService;

    @Test
    void runOkTest() {

        Logger logger = Logger.getLogger("InfoLogging");

        String message = "{\"idPA\":\"00595780131\",\"flows\":[{\"identificativoFlusso\":\"2021-07-26AGID_02-S000000001\",\"dataOraFlusso\":1627293600000}]}";
        when(context.getLogger()).thenReturn(logger);

        doReturn(flowService).when(function).getFlowsServiceInstance(logger);

        function.run(message, context);

        verify(context, times(1)).getLogger();
        verify(flowService, times(1)).flowsXmlDownloading(any(), anyString());
    }

    @Test
    void runWithInvalidMessageTest() {

        Logger logger = Logger.getLogger("InfoLogging");

        String message = "invalidMessage";
        when(context.getLogger()).thenReturn(logger);

        function.run(message, context);

        verify(context, times(1)).getLogger();
    }

    @Test
    void getFlowServiceIstanceTest() throws Exception {

        Logger logger = Logger.getLogger("testlogging");

        // test
        FlowsService istance = function.getFlowsServiceInstance(logger);

        assertNotNull(istance);
    }

    @Test
    void runExceptionTest() {

        Logger logger = Logger.getLogger("InfoLogging");

        String message = "{\"idPA\":\"00595780131\",\"flows\":[{\"identificativoFlusso\":\"2021-07-26AGID_02-S000000001\",\"dataOraFlusso\":1627293600000}]}";
        when(context.getLogger()).thenReturn(logger);

        doThrow(RuntimeException.class).when(function).getFlowsServiceInstance(logger);

        function.run(message, context);

        verify(context, times(1)).getLogger();
    }
}
