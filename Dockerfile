FROM --platform=linux/amd64 adoptopenjdk/openjdk11:jdk-11.0.9.1_1-alpine
ARG JAR_FILE=./target/gpd*.jar

EXPOSE 8085

COPY ${JAR_FILE} /home/application.jar
ENTRYPOINT ["java", "-Dserver.port=8085", "-jar", "/home/application.jar"]
