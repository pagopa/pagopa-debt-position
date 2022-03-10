package it.gov.pagopa.reporting;

import com.azure.storage.blob.models.BlobStorageException;
import com.microsoft.azure.functions.ExecutionContext;
import com.microsoft.azure.functions.HttpRequestMessage;
import com.microsoft.azure.functions.HttpResponseMessage;
import com.microsoft.azure.functions.HttpStatus;
import it.gov.pagopa.reporting.service.FlowsService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Optional;
import java.util.logging.Logger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class GetFlowTest {

    @Spy
    GetFlow function;

    @Mock
    ExecutionContext context;

    @Mock
    FlowsService flowsService;

    @Test
    void runOK() throws Exception {

        // general var
        Logger logger = Logger.getLogger("testlogging");
        String organizationId = "90000000000";
        String flowId = "2022-01-12PPAYITR1XXX-S239349322";
        String flowDate = "2022-01-11T23:31:05";

        // precondition
        when(context.getLogger()).thenReturn(logger);
        doReturn(flowsService).when(function).getFlowsServiceInstance(logger);
        when(flowsService.getByFlow(organizationId, flowId, flowDate)).thenReturn("");

        final HttpResponseMessage.Builder builder = mock(HttpResponseMessage.Builder.class);
        HttpRequestMessage<Optional<String>> request = mock(HttpRequestMessage.class);

        doReturn(builder).when(request).createResponseBuilder(any(HttpStatus.class));
        doReturn(builder).when(builder).header(anyString(), anyString());
        doReturn(builder).when(builder).body(anyString());

        HttpResponseMessage responseMock = mock(HttpResponseMessage.class);
        doReturn(HttpStatus.OK).when(responseMock).getStatus();
        doReturn(responseMock).when(builder).build();

        // test
        HttpResponseMessage response = function.run(request, organizationId, flowId, flowDate, context);

        // Asserts
        assertEquals(HttpStatus.OK, response.getStatus());
    }

    @Test
    void runKO() throws Exception {

        // general var
        Logger logger = Logger.getLogger("testlogging");
        String organizationId =  "90000000000";
        String flowId = "2022-01-12PPAYITR1XXX-S239349322";
        String flowDate = "2022-01-11T23:31:05";

        // precondition
        when(context.getLogger()).thenReturn(logger);
        doReturn(flowsService).when(function).getFlowsServiceInstance(logger);
        doThrow(BlobStorageException.class).when(flowsService).getByFlow(anyString(), anyString(), anyString());

        final HttpResponseMessage.Builder builder = mock(HttpResponseMessage.Builder.class);
        HttpRequestMessage<Optional<String>> request = mock(HttpRequestMessage.class);

        doReturn(builder).when(request).createResponseBuilder(any(HttpStatus.class));
        doReturn(builder).when(builder).header(anyString(), anyString());

        HttpResponseMessage responseMock = mock(HttpResponseMessage.class);
        doReturn(HttpStatus.NOT_FOUND).when(responseMock).getStatus();
        doReturn(responseMock).when(builder).build();

        // test
        HttpResponseMessage response = function.run(request, organizationId, flowId, flowDate, context);

        // Asserts
        assertEquals(HttpStatus.NOT_FOUND, response.getStatus());
    }

}
