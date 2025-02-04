# Migrations on the publication-triplestore

If you want to fix something in the frontend or other service in this stack, DO
NOT MAKE ANY MIGRATIONS HERE.

The publication triplestore is only meant to be used as a intermediate for the
producers in the stack. It is used for calculating differences between what's
in the actual triplestore that is used for all the services, and what has
already been sent to the consuming apps in terms of delta files.

It can be used as a trick to re-export certain data. For example, if some
Sumbission is missing from consuming apps, you can remove it and related
subjects from the publication-triplestore and trigger healing on the producer
in question. This will generate the delta files for those subjects, ready to be
consumed. Look at some existing migrations for examples.

