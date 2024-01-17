#
# Build
#
FROM maven:3.8.5-eclipse-temurin:17-jre@sha256:e7ba39bd08d3c1761f610b737164abf964f0784589095fb7dfa1185010a3b70f as buildtime
WORKDIR /build
COPY . .
RUN mvn clean package -Dmaven.test.skip=true

#
# Package stage
#
FROM eclipse-temurin:17-jre@sha256:e7ba39bd08d3c1761f610b737164abf964f0784589095fb7dfa1185010a3b70f as builder
COPY --from=buildtime /build/target/*.jar application.jar
RUN java -Djarmode=layertools -jar application.jar extract


FROM ghcr.io/pagopa/docker-base-springboot-openjdk17:v1.1.0@sha256:6fa320d452fa22066441f1ef292d15eb06f944bc8bca293e1a91ea460d30a613

COPY --chown=spring:spring  --from=builder dependencies/ ./
COPY --chown=spring:spring  --from=builder snapshot-dependencies/ ./
# https://github.com/moby/moby/issues/37965#issuecomment-426853382
RUN true
COPY --chown=spring:spring  --from=builder spring-boot-loader/ ./
COPY --chown=spring:spring  --from=builder application/ ./

EXPOSE 8080

ENTRYPOINT ["java","-jar","application.jar"]