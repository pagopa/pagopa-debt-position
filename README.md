# pagopa-debt-position
PagoPA service to manage EC debtor positions
---
## Api Documentation 📖
### GPD
See the [OpenApi 3 here.](https://editor.swagger.io/?url=https://raw.githubusercontent.com/pagopa/pagopa-debt-position/main/openapi/openapi_internal.json)

In local env typing following url on browser for ui interface: 
```
http://localhost:8080/swagger-ui/index.html

```
or that for `yaml` version
```http://localhost:8080/v3/api-docs/```

---
## Technology Stack
- Java 17
- Spring Boot
- Spring Web
- Hibernate
- JPA

---

## Start Project Locally 🚀

### Prerequisites
- docker

### Run docker container

Under docker project folder typing :
`docker-compose up --build`
>**NOTE** : before that compile `gpd` service with `mvn clean package` command

If all right, eventually you'll see something like that:
```sh
gpd       | 2022-01-27 13:49:00.772  INFO 1 --- [           main] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat started on port(s): 8085 (http) with context path ''
gpd       | 2022-01-27 13:49:00.792  INFO 1 --- [           main] i.g.p.d.DebtPositionApplication          : Started DebtPositionApplication in 9.591 seconds (JVM running for 10.458)
```

---

## Develop Locally 💻

Under `gpd` main project folder typing :

```sh 
bash ./docker/run_local.sh
```
or
```sh 
bash ./docker/run_docker.sh local
```
> **NOTE**: above command run spring boot application via `mvn` command. You can comment this line and runs it with your favourite ide, to debug. 

### Prerequisites
- git
- maven
- jdk-17
- docker

### Run the project
The easiest way to develop locally is start only db container and run spring-boot application.
```
/usr/local/bin/docker-compose up -d postgres
/usr/local/bin/docker-compose up -d flyway
```

### Run the project with h2 database

```
mvn spring-boot:run -Dspring-boot.run.profiles=h2
```

### FlyWay - versioning schema changes
For Spring Boot project:
- Add Flyway as a dependency in pom.xml
<dependency>
    <groupId>org.flywaydb</groupId>
    <artifactId>flyway-core</artifactId>
</dependency>

When this dependency added Spring Boot detects Flyway on the classpath and it will run it on startup.
In this way, by default, flyway looks at files in the format V$X__$DESCRIPTION.sql (where $X is the migration version name) in the folder src/main/resources/db/migration.
Example of the naming convention is: V001__INIT.sql

> **NOTE**: In the application.properties the ddl-auto configuration must be validate. This causes Hibernate to validate the schema to see if it matches with what is defined in Java.

### Testing 🧪

#### Unit testing

Under `gpd` main project folder typing `mvn clean verify`, if all right you'll see following stuffs

```sh
[INFO] Results:
[INFO]
[INFO] Tests run: 11, Failures: 0, Errors: 0, Skipped: 0
[INFO]
[INFO]
[INFO] --- maven-jar-plugin:3.2.0:jar (default-jar) @ debt-position ---
[INFO] Building jar: /Users/pasqualespica/my_data/__TEMP/pagopa-debt-position/gpd/target/debt-position-0.0.1-SNAPSHOT.jar
[INFO]
[INFO] --- spring-boot-maven-plugin:2.6.2:repackage (repackage) @ debt-position ---
[INFO] Replacing main artifact with repackaged archive
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
```

#### Integration testing

under main `gpd` project folder typing

```sh
 integration-test/run_integration_test.sh local
```
> **NOTE**: suppose `Started DebtPositionApplication` on port `8080`

#### Load testing

under main `gpd` folder typing

```sh
 bash api-test/run_test.sh l load
```
> **NOTE**: suppose `Started DebtPositionApplication` on port `8085`


---

### Mainteiners
See `CODEOWNERS` file



