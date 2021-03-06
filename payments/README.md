# Payments

- [Payments](#payments)
  * [Api Documentation ๐](#api-documentation---)
  * [Technology Stack ๐](#technology-stack---)
  * [Start Project Locally ๐](#start-project-locally---)
    + [Prerequisites](#prerequisites)
    + [Run docker container](#run-docker-container)
  * [Develop Locally ๐ป](#develop-locally---)
    + [Prerequisites](#prerequisites-1)
    + [Run the project](#run-the-project)
  * [Testing ๐งช](#testing---)
    - [Unit testing](#unit-testing)
    - [Integration testing](#integration-testing)
    - [Load testing](#load-testing)
  * [Mainteiners ๐จโ๐ป](#mainteiners------)
  
---
## Api Documentation ๐
See the [OpenApi 3 here.](https://editor.swagger.io/?url=https://raw.githubusercontent.com/pagopa/pagopa-debt-position/main/payments/openapi/openapi.json)

---

## Technology Stack ๐

- Java 11
- Spring Boot
- Spring Web
- Feign Client

---  

## Start Project Locally ๐

### Prerequisites

- docker
- a runnign GPD mock (see `mock` folder of this repository)

### Run docker container

Under `payments` folder typing:

`docker-compose up --build`

---

## Develop Locally ๐ป

### Prerequisites

- git
- maven
- jdk-11
- docker

### Run the project

Under `payments` folder typing:

`mvn spring-boot:run -Dspring-boot.run.profiles=local`

---

## Testing ๐งช

### Prerequisites

- maven
- [newman](https://www.npmjs.com/package/newman)
- [postman-to-k6](https://github.com/apideck-libraries/postman-to-k6)
- [k6](https://k6.io/)

### Unit testing

Under `payments` folder typing:

`mvn clean verify`

### Integration testing

Under `payments` folder typing:

```sh
 newman run api-test/GPD.postman_collection.json --environment=api-test/local.postman_environment.json 
```

> **NOTE**: suppose `Started Payments` on port `8080`

### Load testing

Under `payments` folder typing:

```sh
postman-to-k6 api-test/GPD.postman_collection.json --environment api-test/local.postman_environment.json -o ./k6-script.js
k6 run --vus 2 --duration 30s ./k6-script.js
```

> **NOTE**: suppose `Started Payments` on port `8085`

---

## Mainteiners ๐จโ๐ป

See `CODEOWNERS` file



