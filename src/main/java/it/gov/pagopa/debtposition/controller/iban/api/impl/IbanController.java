package it.gov.pagopa.debtposition.controller.iban.api.impl;

import it.gov.pagopa.debtposition.controller.iban.api.IIbanController;
import it.gov.pagopa.debtposition.model.iban.UpdateIbanMassiveModel;
import it.gov.pagopa.debtposition.service.iban.IbanService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import javax.validation.Valid;

@Controller
@Slf4j
public class IbanController implements IIbanController {
    private static final String LOG_BASE_HEADER_INFO =
            "[RequestMethod: %s] - [ClassMethod: %s] - [MethodParamsToLog: %s]";
    private static final String LOG_BASE_PARAMS_DETAIL = "organizationFiscalCode= %s; oldIban= %s; newIban= %s";

    private final IbanService ibanService;

    @Autowired
    public IbanController(IbanService ibanService) {
        this.ibanService = ibanService;
    }


    @Override
    public void updateIbanMassive(
            String organizationFiscalCode,
            @Valid UpdateIbanMassiveModel updateIbanMassiveModel) {
        log.debug(
                String.format(
                        LOG_BASE_HEADER_INFO,
                        "POST",
                        "updateIbanMassive",
                        String.format(
                                LOG_BASE_PARAMS_DETAIL,
                                organizationFiscalCode,
                                updateIbanMassiveModel.getOldIban(),
                                updateIbanMassiveModel.getNewIban())));

        ibanService.updateIbanMassive(organizationFiscalCode, updateIbanMassiveModel.getOldIban(), updateIbanMassiveModel.getNewIban());
    }

}