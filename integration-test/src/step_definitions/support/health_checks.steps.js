const { Given } = require('@cucumber/cucumber');
const { executeHealthCheckForGPD } = require('./logic/health_checks_logic');

Given('GPD running', () => executeHealthCheckForGPD());