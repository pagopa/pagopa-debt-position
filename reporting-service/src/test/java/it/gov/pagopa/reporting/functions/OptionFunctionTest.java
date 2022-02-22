package it.gov.pagopa.reporting.functions;

import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
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

import it.gov.pagopa.reporting.service.OptionsService;

@ExtendWith(MockitoExtension.class)
class OptionFunctionTest {

    @Mock
    ExecutionContext context;

    @Mock
    OptionsService optionsService;

    @Spy
    UpdateOption function;

//    @Test
//    void runOkTest() {
//
//        Logger logger = Logger.getLogger("InfoLogging");
//
//        String message = "{\"idFlow\":\"00595780131\",\"iuvs\":[\"1627293600000\"],\"dateFlow\":1627293600000}";
//        when(context.getLogger()).thenReturn(logger);
//
//        doReturn(optionsService).when(function).getOptionsServiceInstance(logger);
//
//        function.run(message, context);
//
//        verify(context, times(1)).getLogger();
//        verify(optionsService, times(1)).callGPDServiceToReportOption(any());
//    }

    @Test
    void runWithInvalidMessageTest() {

        Logger logger = Logger.getLogger("InfoLogging");

        String message = "invalidMessage";
        when(context.getLogger()).thenReturn(logger);

        function.run(message, context);

        verify(context, times(1)).getLogger();
    }

//    @Test
//    void getOptionsServiceIstanceTest() throws Exception {
//
//        Logger logger = Logger.getLogger("testlogging");
//
//        // test
//        OptionsService istance = function.getOptionsServiceInstance(logger);
//
//        assertNotNull(istance);
//    }

//    @Test
//    void runExceptionTest() {
//
//        Logger logger = Logger.getLogger("InfoLogging");
//
//        String message = "{\"idFlow\":\"00595780131\",\"iuvs\":[\"1627293600000\"],\"dateFlow\":1627293600000}";
//        when(context.getLogger()).thenReturn(logger);
//
//        doThrow(RuntimeException.class).when(function).getOptionsServiceInstance(logger);
//
//        function.run(message, context);
//
//        verify(context, times(1)).getLogger();
//    }
}
