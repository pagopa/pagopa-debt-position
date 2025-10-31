package it.gov.pagopa.debtposition.mapper.utils;

import jakarta.annotation.PostConstruct;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class UtilityMapperBootstrap {

    @Value("${database.migration.fields.read.from:READ_FROM_PAYMENT_POSITION}")
    private String readFrom;

    @PostConstruct
    public void init() {
        UtilityMapper.setReadFrom(readFrom);
    }
}