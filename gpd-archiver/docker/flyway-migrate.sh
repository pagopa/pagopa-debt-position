# Local testing only
export $(cat ../.env | xargs)

flyway migrate \
  -url=${FLYWAY_URL} \
  -user=${FLYWAY_USER} \
  -password=${FLYWAY_PASSWORD} \
  -schemas=apd,partman \
  -locations=filesystem:../../src/main/resources/apd_archive