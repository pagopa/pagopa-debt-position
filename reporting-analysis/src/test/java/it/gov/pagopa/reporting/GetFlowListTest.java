package it.gov.pagopa.reporting;

import com.microsoft.azure.functions.ExecutionContext;
import com.microsoft.azure.functions.HttpRequestMessage;
import com.microsoft.azure.functions.HttpResponseMessage;
import com.microsoft.azure.functions.HttpStatus;
import it.gov.pagopa.reporting.model.Flow;
import it.gov.pagopa.reporting.service.FlowsService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

import java.security.InvalidKeyException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.logging.Logger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class GetFlowListTest {

    @Spy
    GetFlowList function;

    @Mock
    ExecutionContext context;

    @Mock
    FlowsService flowsService;

    @Test
    void runOK_withFlowDate() throws Exception {

        // general var
        Logger logger = Logger.getLogger("testlogging");
        List<Flow> flowList = new ArrayList<>();
        String organizationId =  "90000000000";

        // precondition
        when(context.getLogger()).thenReturn(logger);
        doReturn(flowsService).when(function).getFlowsServiceInstance(logger);
        doReturn(flowList).when(flowsService).getByOrganization(organizationId, "2022-01-01");

        final HttpResponseMessage.Builder builder = mock(HttpResponseMessage.Builder.class);
        HttpRequestMessage<Optional<String>> request = mock(HttpRequestMessage.class);

        doReturn(builder).when(request).createResponseBuilder(any(HttpStatus.class));
        doReturn(builder).when(builder).header(anyString(), anyString());
        doReturn(builder).when(builder).body(anyString());

        HttpResponseMessage responseMock = mock(HttpResponseMessage.class);
        doReturn(HttpStatus.OK).when(responseMock).getStatus();
        doReturn(responseMock).when(builder).build();

        // test
        HttpResponseMessage response = function.run(request, organizationId, context);

        // Asserts
        assertEquals(HttpStatus.OK, response.getStatus());
    }

    @Test
    void runOK_noFlowDate() throws Exception {

        // general var
        Logger logger = Logger.getLogger("testlogging");
        List<Flow> flowList = new ArrayList<>();
        String organizationId =  "90000000000";

        // precondition
        when(context.getLogger()).thenReturn(logger);
        doReturn(flowsService).when(function).getFlowsServiceInstance(logger);
        doReturn(flowList).when(flowsService).getByOrganization(organizationId, null);

        final HttpResponseMessage.Builder builder = mock(HttpResponseMessage.Builder.class);
        HttpRequestMessage<Optional<String>> request = mock(HttpRequestMessage.class);

        doReturn(builder).when(request).createResponseBuilder(any(HttpStatus.class));
        doReturn(builder).when(builder).header(anyString(), anyString());
        doReturn(builder).when(builder).body(anyString());

        HttpResponseMessage responseMock = mock(HttpResponseMessage.class);
        doReturn(HttpStatus.OK).when(responseMock).getStatus();
        doReturn(responseMock).when(builder).build();

        // test
        HttpResponseMessage response = function.run(request, organizationId, context);

        // Asserts
        assertEquals(HttpStatus.OK, response.getStatus());
    }


    @Test
    void runKO() throws Exception {

        // general var
        Logger logger = Logger.getLogger("testlogging");
        String organizationId =  "90000000000";

        // precondition
        when(context.getLogger()).thenReturn(logger);
        doReturn(flowsService).when(function).getFlowsServiceInstance(logger);
        doThrow(InvalidKeyException.class).when(flowsService).getByOrganization(organizationId, "2022-01-01");

        final HttpResponseMessage.Builder builder = mock(HttpResponseMessage.Builder.class);
        HttpRequestMessage<Optional<String>> request = mock(HttpRequestMessage.class);

        doReturn(builder).when(request).createResponseBuilder(any(HttpStatus.class));
        doReturn(builder).when(builder).header(anyString(), anyString());

        HttpResponseMessage responseMock = mock(HttpResponseMessage.class);
        doReturn(HttpStatus.BAD_REQUEST).when(responseMock).getStatus();
        doReturn(responseMock).when(builder).build();

        // test
        HttpResponseMessage response = function.run(request, organizationId, context);

        // Asserts
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatus());
    }

}
