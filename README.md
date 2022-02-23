# pagopa-debt-position
PagoPA service to manage EC debtor positions
---
## Api Documentation 📖

See the [OpenApi 3 here.](https://editor.swagger.io/?url=https://raw.githubusercontent.com/pagopa/pagopa-debt-position/main/gpd/openapi/openapi.json)

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
>**NOTE** : before that compile `gpd` service with `mvn clean package` command

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
/usr/local/bin/docker-compose up -d postgres
/usr/local/bin/docker-compose up -d flyway
```


## Start the dev environment for reporting subsystem

### Docker

From `reporting-batch` folder:

```
mv .env.example .env
```

From `reporting-service` folder:

```
mv .env.example .env
```

From the project root:
```
docker-compose -f docker-compose-reporting.yml up --build
```

### Local
By default, Azurite will listen for the :
- [Blob service on port 10000](
https://docs.microsoft.com/en-us/azure/storage/common/storage-use-azurite?tabs=visual-studio#blob-listening-port-configuration)
- [Queue service on port 10001](https://docs.microsoft.com/en-us/azure/storage/common/storage-use-azurite?tabs=visual-studio#queue-listening-port-configuration)
- [Table service on port 10002](https://docs.microsoft.com/en-us/azure/storage/common/storage-use-azurite?tabs=visual-studio#table-listening-port-configuration)

```
docker run -p 10000:10000 -p 10001:10001 -p 10002:10002 mcr.microsoft.com/azure-storage/azurite
```

From `reporting-batch` folder:

```
cp local.settings.json.example local.settings.json
```

From `reporting-service` folder:

```
mv local.settings.json.example local.settings.json
```

```
mvn azure-functions:run
```


### Testing 🧪

#### Unit testing

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

#### Integration testing

under `gpd` folder typing

```sh
 bash api-test/run_test.sh l int
```
> **NOTE**: suppose `Started DebtPositionApplication` on port `8085`

if all  right you'll see something like that :

```sh
┌─────────────────────────┬───────────────────┬──────────────────┐
│                         │          executed │           failed │
├─────────────────────────┼───────────────────┼──────────────────┤
│              iterations │                 1 │                0 │
├─────────────────────────┼───────────────────┼──────────────────┤
│                requests │                 9 │                0 │
├─────────────────────────┼───────────────────┼──────────────────┤
│            test-scripts │                18 │                0 │
├─────────────────────────┼───────────────────┼──────────────────┤
│      prerequest-scripts │                10 │                0 │
├─────────────────────────┼───────────────────┼──────────────────┤
│              assertions │                13 │                0 │
├─────────────────────────┴───────────────────┴──────────────────┤
│ total run duration: 1003ms                                     │
├────────────────────────────────────────────────────────────────┤
│ total data received: 5.25kB (approx)                           │
├────────────────────────────────────────────────────────────────┤
│ average response time: 79ms [min: 8ms, max: 207ms, s.d.: 61ms] │
└────────────────────────────────────────────────────────────────┘
```


#### Load testing

under `gpd` folder typing

```sh
 bash api-test/run_test.sh l load
```
> **NOTE**: suppose `Started DebtPositionApplication` on port `8085`


---

### Mainteiners
See `CODEOWNERS` file

