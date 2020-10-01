
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
                                       
Top 10 WRITE Tables
select schemaname as "Schema Name", relname as "Table Name",
n_tup_ins+n_tup_upd+n_tup_del as "no.of writes" from
pg_stat_all_tables where schemaname not in ('snapshots','pg_catalog')
order by n_tup_ins+n_tup_upd+n_tup_del desc limit 10;
                                       
Top 10 READ Tables
SELECT schemaname as "Schema Name", relname as "Table
Name",seq_tup_read+idx_tup_fetch as "no. of reads" FROM
pg_stat_all_tables WHERE (seq_tup_read + idx_tup_fetch) > 0 and
schemaname NOT IN ('snapshots','pg_catalog') ORDER BY
seq_tup_read+idx_tup_fetch desc limit 10;
                                       
Largest Tables in DB
SELECT QUOTE_IDENT(TABLE_SCHEMA)||'.'||QUOTE_IDENT(table_name) as
table_name,pg_relation_size(QUOTE_IDENT(TABLE_SCHEMA)||'.'||QUOTE_IDENT(table_name)) as size,
pg_total_relation_size(QUOTE_IDENT(TABLE_SCHEMA)||'.'||QUOTE_IDENT(table_name)) as total_size,
pg_size_pretty(pg_relation_size(QUOTE_IDENT(TABLE_SCHEMA)||'.'||QUOTE_IDENT(table_name))) as pretty_relation_size,
pg_size_pretty(pg_total_relation_size(QUOTE_IDENT(TABLE_SCHEMA)||'.'||QUOTE_IDENT(table_name))) as pretty_total_relation_size 
FROM information_schema.tables WHERE QUOTE_IDENT(TABLE_SCHEMA) NOT IN ('snapshots') ORDER BY size DESC LIMIT 10;
                                       
DB Size
SELECT datname, pg_database_size(datname),
pg_size_pretty(pg_database_size(datname))
FROM pg_database
ORDER BY 2 DESC;
                                
Table Size
SELECT schemaname, relname, pg_total_relation_size(schemaname
|| '.' || relname ) ,
pg_size_pretty(pg_total_relation_size(schemaname || '.' ||
relname ))
FROM pg_stat_user_tables
ORDER BY 3 DESC;
                                      
Index Size
SELECT schemaname, relname, indexrelname,
pg_total_relation_size(schemaname || '.' || indexrelname ) ,
pg_size_pretty(pg_total_relation_size(schemaname || '.' ||
indexrelname ))
FROM pg_stat_user_indexes
ORDER BY 1,2,3,4 DESC;
                              
Index Utilization
SELECT schemaname, relname, indexrelname, idx_scan, idx_tup_fetch,
idx_tup_read
FROM pg_stat_user_indexes
ORDER BY 4 DESC,1,2,3;
                                      
Tables That Are Being Updated the Most and Looking for VACUUM
select relname, /* pg_size_pretty( pg_relation_size( relid ) ) as table_size,
                 pg_size_pretty( pg_total_relation_size( relid ) ) as table_total_size, */
                 n_tup_upd, n_tup_hot_upd, n_live_tup, n_dead_tup, last_vacuum::date, last_autovacuum::date, last_analyze::date, last_autoanalyze::date
from pg_stat_all_tables
where relid in (select oid from pg_class
                       where relnamespace not in (select oid from pg_namespace
                               where nspname in ('information_schema', 'pg_catalog','pg_toast', 'edbhc' ) ) )
order by n_tup_upd desc, schemaname, relname;
SELECT schemaname,
         relname,
         now() - last_autovacuum AS "noautovac",
         now() - last_vacuum AS "novac",
         n_tup_upd,
         n_tup_del,
         autovacuum_count,
         last_autovacuum,
         vacuum_count,
         last_vacuum
FROM pg_stat_user_tables
WHERE (now() - last_autovacuum > '7 days'::interval
        AND now() - last_vacuum >'7 days'::interval)
        OR (last_autovacuum IS NULL AND last_vacuum IS NULL ) AND n_dead_tup > 0
ORDER BY  novac DESC;
SELECT relname, n_live_tup, n_dead_tup, trunc(100*n_dead_tup/(n_live_tup+1))::float "ratio%",
to_char(last_autovacuum, 'YYYY-MM-DD HH24:MI:SS') as autovacuum_date,
to_char(last_autoanalyze, 'YYYY-MM-DD HH24:MI:SS') as autoanalyze_date
FROM pg_stat_all_tables where schemaname not in ('pg_toast','pg_catalog','information_schema')
ORDER BY last_autovacuum ;
Bloated Index to Run Reindexing (Locking Operation)\pgrepack (Online Rebuilding)
SELECT current_database(), nspname AS schemaname, tblname, idxname, bs*(relpages)::bigint AS real_size,
  bs*(relpages-est_pages)::bigint AS extra_size,
  100 * (relpages-est_pages)::float / relpages AS extra_ratio,
  fillfactor, bs*(relpages-est_pages_ff) AS bloat_size,
  100 * (relpages-est_pages_ff)::float / relpages AS bloat_ratio,
  is_na
  -- , 100-(sub.pst).avg_leaf_density, est_pages, index_tuple_hdr_bm, maxalign, pagehdr, nulldatawidth, nulldatahdrwidth, sub.reltuples, sub.relpages -- (DEBUG INFO)
FROM (
  SELECT coalesce(1 +
       ceil(reltuples/floor((bs-pageopqdata-pagehdr)/(4+nulldatahdrwidth)::float)), 0 -- ItemIdData size + computed avg size of a tuple (nulldatahdrwidth)
    ) AS est_pages,
    coalesce(1 +
       ceil(reltuples/floor((bs-pageopqdata-pagehdr)*fillfactor/(100*(4+nulldatahdrwidth)::float))), 0
    ) AS est_pages_ff,
    bs, nspname, table_oid, tblname, idxname, relpages, fillfactor, is_na
    -- , stattuple.pgstatindex(quote_ident(nspname)||'.'||quote_ident(idxname)) AS pst, index_tuple_hdr_bm, maxalign, pagehdr, nulldatawidth, nulldatahdrwidth, 
             reltuples -- (DEBUG INFO)
  FROM (
    SELECT maxalign, bs, nspname, tblname, idxname, reltuples, relpages, relam, table_oid, fillfactor,
      ( index_tuple_hdr_bm +
          maxalign - CASE -- Add padding to the index tuple header to align on MAXALIGN
            WHEN index_tuple_hdr_bm%maxalign = 0 THEN maxalign
            ELSE index_tuple_hdr_bm%maxalign
          END
        + nulldatawidth + maxalign - CASE -- Add padding to the data to align on MAXALIGN
            WHEN nulldatawidth = 0 THEN 0
            WHEN nulldatawidth::integer%maxalign = 0 THEN maxalign
            ELSE nulldatawidth::integer%maxalign
          END
      )::numeric AS nulldatahdrwidth, pagehdr, pageopqdata, is_na
      -- , index_tuple_hdr_bm, nulldatawidth -- (DEBUG INFO)
    FROM (
      SELECT
        i.nspname, i.tblname, i.idxname, i.reltuples, i.relpages, i.relam, a.attrelid AS table_oid,
        current_setting('block_size')::numeric AS bs, fillfactor,
        CASE -- MAXALIGN: 4 on 32bits, 8 on 64bits (and mingw32 ?)
          WHEN version() ~ 'mingw32' OR version() ~ '64-bit|x86_64|ppc64|ia64|amd64' THEN 8
          ELSE 4
        END AS maxalign,
        /* per page header, fixed size: 20 for 7.X, 24 for others */
        24 AS pagehdr,
        /* per page btree opaque data */
        16 AS pageopqdata,
        /* per tuple header: add IndexAttributeBitMapData if some cols are null-able */
        CASE WHEN max(coalesce(s.null_frac,0)) = 0
          THEN 2 -- IndexTupleData size
          ELSE 2 + (( 32 + 8 - 1 ) / 8) -- IndexTupleData size + IndexAttributeBitMapData size ( max num filed per index + 8 - 1 /8)
        END AS index_tuple_hdr_bm,
        /* data len: we remove null values save space using it fractionnal part from stats */
        sum( (1-coalesce(s.null_frac, 0)) * coalesce(s.avg_width, 1024)) AS nulldatawidth,
        max( CASE WHEN a.atttypid = 'pg_catalog.name'::regtype THEN 1 ELSE 0 END ) > 0 AS is_na
      FROM pg_attribute AS a
        JOIN (
          SELECT nspname, tbl.relname AS tblname, idx.relname AS idxname, idx.reltuples, idx.relpages, idx.relam,
            indrelid, indexrelid, indkey::smallint[] AS attnum,
            coalesce(substring(
              array_to_string(idx.reloptions, ' ')
               from 'fillfactor=([0-9]+)')::smallint, 90) AS fillfactor
          FROM pg_index
            JOIN pg_class idx ON idx.oid=pg_index.indexrelid
            JOIN pg_class tbl ON tbl.oid=pg_index.indrelid
            JOIN pg_namespace ON pg_namespace.oid = idx.relnamespace
          WHERE pg_index.indisvalid AND tbl.relkind = 'r' AND idx.relpages > 0
        ) AS i ON a.attrelid = i.indexrelid
        JOIN pg_stats AS s ON s.schemaname = i.nspname
          AND ((s.tablename = i.tblname AND s.attname = pg_catalog.pg_get_indexdef(a.attrelid, a.attnum, TRUE)) -- stats from tbl
          OR   (s.tablename = i.idxname AND s.attname = a.attname))-- stats from functionnal cols
        JOIN pg_type AS t ON a.atttypid = t.oid
      WHERE a.attnum > 0
      GROUP BY 1, 2, 3, 4, 5, 6, 7, 8, 9
    ) AS s1
  ) AS s2
    JOIN pg_am am ON s2.relam = am.oid WHERE am.amname = 'btree'
) AS sub
-- WHERE NOT is_na
ORDER BY 2,3,4;                                     
