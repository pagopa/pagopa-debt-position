# Local testing only
export $(cat ../.env | xargs)

flyway migrate \
  -url=jdbc:postgresql://localhost:5432/io-apd \
  -user=${FLYWAY_USER} \
  -password=${FLYWAY_PASSWORD} \
  -schemas=apd,partman \
  -locations=filesystem:../sql-flyway