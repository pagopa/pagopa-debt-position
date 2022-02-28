package it.gov.pagopa.debtposition.controller.configuration.api.impl;

import java.time.LocalDate;
import java.util.List;

import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

import it.gov.pagopa.debtposition.controller.configuration.api.IConfigurationsController;
import it.gov.pagopa.debtposition.model.payments.OrganizationModelQueryBean;
import it.gov.pagopa.debtposition.model.payments.response.OrganizationListModelResponse;
import it.gov.pagopa.debtposition.model.payments.response.OrganizationModelResponse;
import it.gov.pagopa.debtposition.service.configurations.ConfigurationsService;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import lombok.extern.slf4j.Slf4j;

@Controller
@Slf4j
public class ConfigurationsController implements IConfigurationsController {
	
	@Autowired
	private ConfigurationsService configurationsService;
	
	private static final String LOG_BASE_HEADER_INFO   = "[RequestMethod: %s] - [ClassMethod: %s] - [MethodParamsToLog: %s]";
	

	@Override
	public ResponseEntity<OrganizationListModelResponse> getOrganizations(@Valid LocalDate since) {
		log.info(String.format(LOG_BASE_HEADER_INFO,"GET","getOrganizations"," since="+since));
		List<OrganizationModelQueryBean> ppListToAdd = configurationsService.getOrganizationsToAdd(since);
		List<OrganizationModelQueryBean> ppListToDelete = configurationsService.getOrganizationsToDelete(since);
				
		// flip bean to model
		List<OrganizationModelResponse> ppToAddResponseList = ObjectMapperUtils.mapAll(ppListToAdd, OrganizationModelResponse.class);
		List<OrganizationModelResponse> ppToDeleteResponseList = ObjectMapperUtils.mapAll(ppListToDelete, OrganizationModelResponse.class);
		
		return new ResponseEntity<>(OrganizationListModelResponse.builder()
		.add(ppToAddResponseList)
		.delete(ppToDeleteResponseList)
		.build(), HttpStatus.OK);
	}


	@Override
	public ResponseEntity<String> checkOrganization(String organizationFiscalCode) {
		log.info(String.format(LOG_BASE_HEADER_INFO,"GET","checkOrganization"," organizationFiscalCode="+organizationFiscalCode));
		configurationsService.checkOrganization(organizationFiscalCode);
		return new ResponseEntity<>(HttpStatus.OK);
	}
	
}
