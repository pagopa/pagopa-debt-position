#
# Build
#
FROM maven:3.8.5-amazoncorretto-17-al2023@sha256:eeaa7ab572d931f7273fc5cf31429923f172091ae388969e11f42ec6dd817d74 as buildtime
WORKDIR /build
COPY . .
RUN mvn clean package -Dmaven.test.skip=true

#
# Package stage
#
FROM amazoncorretto:17.0.9-alpine3.18@sha256:df48bf2e183230040890460ddb4359a10aa6c7aad24bd88899482c52053c7e17 as builder
COPY --from=buildtime /build/target/*.jar application.jar
RUN java -Djarmode=layertools -jar application.jar extract


FROM ghcr.io/pagopa/docker-base-springboot-openjdk11:v1.0.1@sha256:bbbe948e91efa0a3e66d8f308047ec255f64898e7f9250bdb63985efd3a95dbf

COPY --chown=spring:spring  --from=builder dependencies/ ./
COPY --chown=spring:spring  --from=builder snapshot-dependencies/ ./
# https://github.com/moby/moby/issues/37965#issuecomment-426853382
RUN true
COPY --chown=spring:spring  --from=builder spring-boot-loader/ ./
COPY --chown=spring:spring  --from=builder application/ ./

EXPOSE 8080