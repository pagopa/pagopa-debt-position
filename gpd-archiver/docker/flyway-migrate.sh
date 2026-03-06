# Local testing only
export $(cat ../.dev.env | xargs)

flyway migrate \
  -url=${FLYWAY_URL} \
  -user=${FLYWAY_USER} \
  -password=${FLYWAY_PASSWORD} \
  -schemas=apd,partman \
  -locations=filesystem:../../src/main/resources/db/migration/apd_archive