# Dockerfile
FROM postgres:16-bookworm

# Installing extensions from the official Debian repository
RUN apt-get update && apt-get install -y \
    postgresql-16-partman \
    postgresql-16-cron \
    && rm -rf /var/lib/apt/lists/*

# Automatic configuration of parameters required at startup
RUN echo "shared_preload_libraries = 'pg_partman_bgw, pg_cron'" >> /usr/share/postgresql/postgresql.conf.sample