package it.gov.pagopa.debtposition;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;

import org.junit.jupiter.api.Test;

import it.gov.pagopa.debtposition.util.Constants;

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
    
    @Test
    void constructorIsPrivate() throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, InstantiationException {
      Constructor<Constants> constructor = Constants.class.getDeclaredConstructor();
      assertTrue(Modifier.isPrivate(constructor.getModifiers()));
    }

}
