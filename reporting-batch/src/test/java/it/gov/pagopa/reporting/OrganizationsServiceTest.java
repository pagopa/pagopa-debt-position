package it.gov.pagopa.reporting;

import it.gov.pagopa.reporting.models.Organizations;
import it.gov.pagopa.reporting.service.OrganizationsService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.times;

@ExtendWith(MockitoExtension.class)
public class OrganizationsServiceTest {

  @Test
  void processOrganizationListTest() throws Exception {

    OrganizationsService organizationsService = Mockito.spy(
            new OrganizationsService(
                    "connectionStringMock",
                    "tableMock",
                    "queueMock",
                    Logger.getLogger("testlogging")));

    Organizations orgs = new Organizations();

    List<String> added = new ArrayList<>();
    added.add("90000000001");
    added.add("90000000002");
    added.add("90000000003");
    orgs.setAdd(added);
    List<String> deleted = new ArrayList<>();
    deleted.add("90000000004");
    deleted.add("90000000005");
    orgs.setDelete(deleted);

    /**
     * Precondition
     */


    /**
     * Test
     */
    organizationsService.processOrganizationList(orgs);

    /**
     * Asserts
     */
    verify(organizationsService, times(2)).addOrganizationList(any());
    verify(organizationsService, times(1)).deleteOrganizationList(any());



  }

}
