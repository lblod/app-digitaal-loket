# Motivation
Sometimes we need data to develop on, that should not be in production.
This folder contains these migrations.
## how?
It is not configured to run by default, even in the `docker-compose.dev.yml`.
You'll have to run them manually, or pick a subset as you please.
### why not running by default in development-mode?
In the past, dev migrations and normal migrations ran sometimes out of sync, confusing development tremendously.
