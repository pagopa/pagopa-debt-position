# Dockerfile
FROM postgres:16-bookworm@sha256:da514b7d293c5e9126503f85ecd835f4fb0942a77e012fe74f016c114c3e25b8

# Installing extensions from the official Debian repository
RUN apt-get update && apt-get install -y \
    postgresql-16-partman \
    postgresql-16-cron \
    && rm -rf /var/lib/apt/lists/*

# Automatic configuration of parameters required at startup
RUN echo "shared_preload_libraries = 'pg_partman_bgw, pg_cron'" >> /usr/share/postgresql/postgresql.conf.sample