package it.gov.pagopa.payments.controller;

import it.gov.pagopa.payments.model.AppInfo;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

@RestController()
public class BaseController {
    @Value("${application.name}")
    private String name;

    @Value("${application.version}")
    private String version;

    @Value("${properties.environment}")
    private String environment;


    @GetMapping(value = "/info")
    @ResponseStatus(HttpStatus.OK)
    public ResponseEntity<AppInfo> healthCheck() {
        // Used just for heartbeating
        AppInfo info = AppInfo.builder()
                .name(name)
                .version(version)
                .environment(environment)
                .build();
        return ResponseEntity.status(HttpStatus.OK).body(info);
    }
}
