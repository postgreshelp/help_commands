
pgbadger pre-requisites
------------------------
log_checkpoints = on
log_connections = on
log_disconnections = on
log_lock_waits = on
log_temp_files = 0
log_autovacuum_min_duration = 0
log_error_verbosity = default
log_statement = off
lc_messages='C'
 
Log_min_duration_statement = (see below)

log_line_prefix = '%t [%p]: [%l-1] user=%u,db=%d,app=%a,client=%h '

To Check Version
select version();

To Check Size of Database
SELECT pg_size_pretty(pg_database_size('postgres')) As fulldbsize;

To Get All Catalog Tables
\dt pg_catalog.*
                                       
                                       
