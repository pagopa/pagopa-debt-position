package it.gov.pagopa.debtposition;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import it.gov.pagopa.debtposition.controller.BaseController;
import it.gov.pagopa.debtposition.controller.pd.api.impl.DebtPositionController;
import it.gov.pagopa.debtposition.service.DebtPositionService;

@SpringBootTest
class DebtPositionApplicationTests {
	
	@Autowired
    private BaseController baseController;
	
	@Autowired
    private DebtPositionController debtorPositionController;

    @Autowired
    private DebtPositionService debtPositionService;

	@Test
	void contextLoads() {
		assertThat(baseController).isNotNull();
        assertThat(debtorPositionController).isNotNull();
        assertThat(debtPositionService).isNotNull();
	}

}
