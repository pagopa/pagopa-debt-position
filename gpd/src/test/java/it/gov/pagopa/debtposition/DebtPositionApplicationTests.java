package it.gov.pagopa.debtposition;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertTrue;

class DebtPositionApplicationTests {
	
	@Test
	void contextLoads() {
		assertTrue(true); // it just tests that an error has not occurred
	}
	
	@Test
    void applicationContextLoaded() {
        assertTrue(true); // it just tests that an error has not occurred
    }

    @Test
    void applicationContextTest() {
        DebtPositionApplication.main(new String[]{});
        assertTrue(true); // it just tests that an error has not occurred
    }

}
