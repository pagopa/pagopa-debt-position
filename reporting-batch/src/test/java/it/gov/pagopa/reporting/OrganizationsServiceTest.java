package it.gov.pagopa.reporting;

import com.microsoft.azure.storage.StorageExtendedErrorInformation;
import com.microsoft.azure.storage.table.TableServiceException;
import it.gov.pagopa.reporting.models.Organization;
import it.gov.pagopa.reporting.models.Organizations;
import it.gov.pagopa.reporting.service.OrganizationsService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class OrganizationsServiceTest {

  @Test
  void processOrganizationListTest() throws Exception {

    OrganizationsService organizationsService = Mockito.spy(
            new OrganizationsService(
                    "connectionStringMock",
                    "tableMock",
                    "queueMock",
                    60, 0,
                    Logger.getLogger("testlogging")));

    Organizations orgs = getOrganizations();

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

  @Test
  void processOrganizationListExAddTest() throws Exception {

    OrganizationsService organizationsService = Mockito.spy(
            new OrganizationsService(
                    "connectionStringMock",
                    "tableMock",
                    "queueMock",
                    60, 0,
                    Logger.getLogger("testlogging")));

    Organizations orgs = getOrganizations();

    /**
     * Precondition
     */
    doThrow(new TableServiceException("InvalidDuplicateRow", "message InvalidDuplicateRow", 400,
            new StorageExtendedErrorInformation(), null)).when(organizationsService).addOrganizationList(any());
    doThrow(new TableServiceException("InvalidDuplicateRow", "message InvalidDuplicateRow", 400,
            new StorageExtendedErrorInformation(), null)).when(organizationsService).addOrganization(any());

    //doNothing().when(organizationsService).processOrganizationList(any());

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

  @Test
  void processOrganizationListExDelTest() throws Exception {

    OrganizationsService organizationsService = Mockito.spy(
            new OrganizationsService(
                    "connectionStringMock",
                    "tableMock",
                    "queueMock",
                    60, 0,
                    Logger.getLogger("testlogging")));

    Organizations orgs = getOrganizations();

    /**
     * Precondition
     */
    doThrow(new TableServiceException("InvalidDuplicateRow", "message InvalidDuplicateRow", 400,
            new StorageExtendedErrorInformation(), null)).when(organizationsService).deleteOrganizationList(any());
    doThrow(new TableServiceException("InvalidDuplicateRow", "message InvalidDuplicateRow", 400,
            new StorageExtendedErrorInformation(), null)).when(organizationsService).deleteOrganization(any());

    //doNothing().when(organizationsService).processOrganizationList(any());

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

  private Organizations getOrganizations() {
    Organizations orgs = new Organizations();
    List<Organization> added = new ArrayList<>();
    // this is useful to improve coverage for that case
    Organization o = new Organization();
    o.setOrganizationFiscalCode("90000000001");
    added.add(o);
    added.add(new Organization("90000000002"));
    added.add(new Organization("90000000003"));
    orgs.setAdd(added);
    List<Organization> deleted = new ArrayList<>();
    deleted.add(new Organization("90000000004"));
    deleted.add(new Organization("90000000005"));
    orgs.setDelete(deleted);
    return orgs;
  }

}
