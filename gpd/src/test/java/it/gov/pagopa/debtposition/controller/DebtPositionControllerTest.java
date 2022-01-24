package it.gov.pagopa.debtposition.controller;


import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import it.gov.pagopa.debtposition.DebtPositionApplication;
import it.gov.pagopa.debtposition.TestUtil;
import it.gov.pagopa.debtposition.mock.DebtorDTOMock;
import it.gov.pagopa.debtposition.service.DebtPositionService;


@SpringBootTest(classes = DebtPositionApplication.class)
@AutoConfigureMockMvc
class DebtPositionControllerTest {
	
	@Autowired
    private MockMvc mvc;
	
	@Mock
	private ModelMapper modelMapperMock;
	
	@Mock
	private DebtPositionService debtPositionService;

	
	@BeforeEach
    void setUp() {
        //when(debtPositionService.create(any(Debtor.class), anyString(), anyString()).thenReturn(new Debtor());
    }
	
	/*
	@Test
	void createDebtPosition_201() {

		//ReflectionTestUtils.setField(debtPositionController, "serviceManagementPath", "");

		Debtor debtor = DebtorMock.getMock();
		DebtorDTO debtorDTO = new DebtorDTO();
		
		when(modelMapperMock.map(any(DebtorDTO.class), any())).thenReturn(debtor);

		ResponseEntity<String> result = debtPositionController.createDebtPosition("12345678901", "12345678901IUPDMOCK", debtorDTO);
		assertThat(result.getStatusCode()).isEqualTo(HttpStatus.CREATED);

	}*/
	
	@Test
    void createDebtPosition_201() throws Exception {
		mvc.perform(post("/organizations/12345678901/payment-notices/12345678901IUPDMOCK")
	            .content(TestUtil.toJson(DebtorDTOMock.getMock()))
	            .contentType(MediaType.APPLICATION_JSON))
	    .andExpect(status().isCreated());
	}

    
}
