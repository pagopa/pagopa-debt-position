package it.gov.pagopa.reporting;

import com.microsoft.azure.functions.ExecutionContext;
import it.gov.pagopa.reporting.service.OrganizationsService;
import it.gov.pagopa.reporting.servicewsdl.ObjectFactory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.logging.Logger;

import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class RetrieveOrganizationsTest {

    @Spy
    RetrieveOrganizations function;

    @Mock
    ExecutionContext context;

    @Mock
    OrganizationsService organizationsService;

    ObjectFactory objectFactory = new ObjectFactory();

    @Test
    void runOkTest() throws Exception {

        Logger logger = Logger.getLogger("testlogging");

        // precondition
        when(context.getLogger()).thenReturn(logger);
        doReturn(organizationsService).when(function).getOrganizationsServiceInstance(logger);

        // test
        function.run("ReportingBatchTrigger", context);

        // Asserts
        verify(context, times(1)).getLogger();
        verify(organizationsService, times(1)).getOrganizations();
        verify(organizationsService, times(1)).addToOrganizationsQueue(any());
    }

}
