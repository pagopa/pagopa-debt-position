package it.gov.pagopa.payments.controller;

import org.springframework.http.HttpStatus;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

@RestController()
public class BaseController {
  @GetMapping(value = "/")
  @ResponseStatus(HttpStatus.OK)
  public void heartBeat() {
    // Used just for heartbeating
  }
}
