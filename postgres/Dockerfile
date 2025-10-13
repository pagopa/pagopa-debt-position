#FROM postgres:15.13-alpine@sha256:1414298ea93186123a6dcf872f778ba3bd2347edcbd2f31aa7bb2d9814ff5393
FROM postgres:13.1-alpine@sha256:16af09e480c49226f8e7cd4d602ab27e59f434eb255d2b45bd0cfe1defb86fd7
COPY ./docker-entrypoint-initdb.d/*sql /docker-entrypoint-initdb.d/