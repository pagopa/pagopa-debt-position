# pagopa-debt-position - PagoPA service to manage EC debtor positions
---
## Api Documentation 📖

See the [OpenApi 3 here.](https://editor.swagger.io/?url=https://raw.githubusercontent.com/pagopa/pagopa-debt-position/main/openapi/openapi.json)

---
## Technology Stack
- Java 11
- Spring Boot
- Spring Web
- Hibernate
- JPA

---

## Start Project Locally 🚀

### Prerequisites
- docker

### Run docker container

Under main project folder typing :
`docker-compose up --build`
>**NOTE** : before that compile `gdp` sevice with `mvn package` command

If all right, eventually you'll see something like that:
```sh
gpd       | 2022-01-27 13:49:00.772  INFO 1 --- [           main] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat started on port(s): 8085 (http) with context path ''
gpd       | 2022-01-27 13:49:00.792  INFO 1 --- [           main] i.g.p.d.DebtPositionApplication          : Started DebtPositionApplication in 9.591 seconds (JVM running for 10.458)
```

---

## Develop Locally 💻

Under `gpd` folder typing :

```sh 
bash run_local.sh
```
> **NOTE**: above command run spring boot application via `mvn` command. You can comment this line and runs it with your favourite ide, to debug. 

### Prerequisites
- git
- maven
- jdk-11
- docker

### Run the project
The easiest way to develop locally is start only db container and run spring-boot application.
```
/usr/local/bin/docker-compose up -d oracle
/usr/local/bin/docker-compose up -d flyway
```

### Testing 🧪

Under `gpd` folder typing `mvn clean verify`, if all right you'll see following stuffs

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


#### Unit testing

#### Integration testing

#### Load testing

---

### Mainteiners
See `CODEOWNERS` file

