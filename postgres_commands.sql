
I love :pizza:

-- pgbadger pre-requisites

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

-- To Check Version
select version();

-- To Check Size of Database
SELECT pg_size_pretty(pg_database_size('postgres')) As fulldbsize;

-- To Get All Catalog Tables
\dt pg_catalog.*
                                       
-- long running query
SELECT
  pid,
  now() - pg_stat_activity.query_start AS duration,
  query,
  state
FROM pg_stat_activity
WHERE (now() - pg_stat_activity.query_start) > interval '5 minutes';
                                       
-- table lock                                       
select pid,
       usename,
       pg_blocking_pids(pid) as blocked_by,
       query as blocked_query
from pg_stat_activity
where cardinality(pg_blocking_pids(pid)) > 0;
                                       
-- kill running query
SELECT pg_cancel_backend(procpid);

-- kill idle query
SELECT pg_terminate_backend(procpid);

-- vacuum command
VACUUM (VERBOSE, ANALYZE);

-- all database users
select * from pg_stat_activity where current_query not like '<%';

-- all databases and their sizes
select * from pg_user;

-- all tables and their size, with/without indexes
select datname, pg_size_pretty(pg_database_size(datname))
from pg_database
order by pg_database_size(datname) desc;

-- cache hit rates (should not be less than 0.99)
SELECT sum(heap_blks_read) as heap_read, sum(heap_blks_hit)  as heap_hit, (sum(heap_blks_hit) - sum(heap_blks_read)) / sum(heap_blks_hit) as ratio
FROM pg_statio_user_tables;

-- table index usage rates (should not be less than 0.99)
SELECT relname, 100 * idx_scan / (seq_scan + idx_scan) percent_of_times_index_used, n_live_tup rows_in_table
FROM pg_stat_user_tables 
ORDER BY n_live_tup DESC;

-- how many indexes are in cache
SELECT sum(idx_blks_read) as idx_read, sum(idx_blks_hit)  as idx_hit, (sum(idx_blks_hit) - sum(idx_blks_read)) / sum(idx_blks_hit) as ratio
FROM pg_statio_user_indexes;                                    
                                       
-- Top 10 WRITE Tables
select schemaname as "Schema Name", relname as "Table Name",
n_tup_ins+n_tup_upd+n_tup_del as "no.of writes" from
pg_stat_all_tables where schemaname not in ('snapshots','pg_catalog')
order by n_tup_ins+n_tup_upd+n_tup_del desc limit 10;
                                       
-- Top 10 READ Tables
SELECT schemaname as "Schema Name", relname as "Table
Name",seq_tup_read+idx_tup_fetch as "no. of reads" FROM
pg_stat_all_tables WHERE (seq_tup_read + idx_tup_fetch) > 0 and
schemaname NOT IN ('snapshots','pg_catalog') ORDER BY
seq_tup_read+idx_tup_fetch desc limit 10;
                                       
-- Largest Tables in DB
SELECT QUOTE_IDENT(TABLE_SCHEMA)||'.'||QUOTE_IDENT(table_name) as
table_name,pg_relation_size(QUOTE_IDENT(TABLE_SCHEMA)||'.'||QUOTE_IDENT(table_name)) as size,
pg_total_relation_size(QUOTE_IDENT(TABLE_SCHEMA)||'.'||QUOTE_IDENT(table_name)) as total_size,
pg_size_pretty(pg_relation_size(QUOTE_IDENT(TABLE_SCHEMA)||'.'||QUOTE_IDENT(table_name))) as pretty_relation_size,
pg_size_pretty(pg_total_relation_size(QUOTE_IDENT(TABLE_SCHEMA)||'.'||QUOTE_IDENT(table_name))) as pretty_total_relation_size 
FROM information_schema.tables WHERE QUOTE_IDENT(TABLE_SCHEMA) NOT IN ('snapshots') ORDER BY size DESC LIMIT 10;
                                       
-- DB Size
SELECT datname, pg_database_size(datname),
pg_size_pretty(pg_database_size(datname))
FROM pg_database
ORDER BY 2 DESC;
                                
-- Table Size
SELECT schemaname, relname, pg_total_relation_size(schemaname
|| '.' || relname ) ,
pg_size_pretty(pg_total_relation_size(schemaname || '.' ||
relname ))
FROM pg_stat_user_tables
ORDER BY 3 DESC;
                                      
-- Index Size
SELECT schemaname, relname, indexrelname,
pg_total_relation_size(schemaname || '.' || indexrelname ) ,
pg_size_pretty(pg_total_relation_size(schemaname || '.' ||
indexrelname ))
FROM pg_stat_user_indexes
ORDER BY 1,2,3,4 DESC;
                              
-- Index Utilization
SELECT schemaname, relname, indexrelname, idx_scan, idx_tup_fetch,
idx_tup_read
FROM pg_stat_user_indexes
ORDER BY 4 DESC,1,2,3;
                                      
Get name and value from pg_settings
select name,setting from pg_settings;
Never-Used Indexes
WITH table_scans as (
    SELECT relid,
        tables.idx_scan + tables.seq_scan as all_scans,
        ( tables.n_tup_ins + tables.n_tup_upd + tables.n_tup_del ) as writes,
                pg_relation_size(relid) as table_size
        FROM pg_stat_user_tables as tables
),
all_writes as (
    SELECT sum(writes) as total_writes
    FROM table_scans
),
indexes as (
    SELECT idx_stat.relid, idx_stat.indexrelid,
        idx_stat.schemaname, idx_stat.relname as tablename,
        idx_stat.indexrelname as indexname,
        idx_stat.idx_scan,
        pg_relation_size(idx_stat.indexrelid) as index_bytes,
        indexdef ~* 'USING btree' AS idx_is_btree
    FROM pg_stat_user_indexes as idx_stat
        JOIN pg_index
            USING (indexrelid)
        JOIN pg_indexes as indexes
            ON idx_stat.schemaname = indexes.schemaname
                AND idx_stat.relname = indexes.tablename
                AND idx_stat.indexrelname = indexes.indexname
    WHERE pg_index.indisunique = FALSE
),
index_ratios AS (
SELECT schemaname, tablename, indexname,
    idx_scan, all_scans,
    round(( CASE WHEN all_scans = 0 THEN 0.0::NUMERIC
        ELSE idx_scan::NUMERIC/all_scans * 100 END),2) as index_scan_pct,
    writes,
    round((CASE WHEN writes = 0 THEN idx_scan::NUMERIC ELSE idx_scan::NUMERIC/writes END),2)
        as scans_per_write,
    pg_size_pretty(index_bytes) as index_size,
    pg_size_pretty(table_size) as table_size,
    idx_is_btree, index_bytes
    FROM indexes
    JOIN table_scans
    USING (relid)
),
index_groups AS (
SELECT 'Never Used Indexes' as reason, *, 1 as grp
FROM index_ratios
WHERE
    idx_scan = 0
    and idx_is_btree
UNION ALL
SELECT 'Low Scans, High Writes' as reason, *, 2 as grp
FROM index_ratios
WHERE
    scans_per_write <= 1
    and index_scan_pct < 10
    and idx_scan > 0
    and writes > 100
    and idx_is_btree
UNION ALL
SELECT 'Seldom Used Large Indexes' as reason, *, 3 as grp
FROM index_ratios
WHERE
    index_scan_pct < 5
    and scans_per_write > 1
    and idx_scan > 0
    and idx_is_btree
    and index_bytes > 100000000
UNION ALL
SELECT 'High-Write Large Non-Btree' as reason, index_ratios.*, 4 as grp
FROM index_ratios, all_writes
WHERE
    ( writes::NUMERIC / ( total_writes + 1 ) ) > 0.02
    AND NOT idx_is_btree
    AND index_bytes > 100000000
ORDER BY grp, index_bytes DESC )
SELECT reason, schemaname, tablename, indexname,
    index_scan_pct, scans_per_write, index_size, table_size
FROM index_groups;
Age of DB and Tables
SELECT datname, age(datfrozenxid) FROM pg_database;
SELECT c.oid::regclass as table_name,
       greatest(age(c.relfrozenxid),age(t.relfrozenxid)) as age
FROM pg_class c
LEFT JOIN pg_class t ON c.reltoastrelid = t.oid
WHERE c.relkind IN ('r', 'm');
                                        
-- Grant Privileges on All Tables
SELECT 'grant select,update,usage on '||c.relname||' to username;' FROM pg_catalog.pg_class c
     LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
WHERE c.relkind IN ('r',") AND n.nspname='schemaname' AND pg_catalog.pg_get_userbyid(c.relowner)='username';
Check Privileges on Tables
SELECT n.nspname as "Schema",
  c.relname as "Name",
  CASE c.relkind WHEN 'r' THEN 'table' WHEN 'v' THEN 'view' WHEN 'S' THEN 'sequence' END as "Type",
  pg_catalog.array_to_string(c.relacl, E'\n') AS "Access privileges",
  pg_catalog.array_to_string(ARRAY(
    SELECT attname || E':\n  ' || pg_catalog.array_to_string(attacl, E'\n  ')
    FROM pg_catalog.pg_attribute a
    WHERE attrelid = c.oid AND NOT attisdropped AND attacl IS NOT NULL
  ), E'\n') AS "Column access privileges"
FROM pg_catalog.pg_class c
     LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
WHERE c.relkind IN ('r') AND pg_catalog.pg_get_userbyid(c.relowner)='username' AND n.nspname='schemaname';
Find All Functions with Arguments
SELECT n.nspname || '.' || p.proname || '(' || pg_catalog.oidvectortypes(p.proargtypes) || ')' as FunctionName,usename as OWNER FROM pg_proc p LEFT JOIN pg_catalog.pg_namespace n ON n.oid = p.pronamespace, pg_user u WHERE p.prorettype <> 'pg_catalog.cstring'::pg_catalog.regtype AND p.proargtypes[0] <> 'pg_catalog.cstring'::pg_catalog.regtype AND pg_catalog.pg_function_is_visible(p.oid) AND p.proowner=u.usesysid AND n.nspname not in ('pg_catalog','sys');
select prona.me||'('||pg_get_function_arguments(pg_proc.oid)||')' as function_arguments,usename,nspname from pg_proc,pg_user,pg_namespace where  proowner=pg_user.usesysid and pronamespace=pg_namespace.oid and usename<>nspname and nspname !~ '^pg_catalog|^information_schema|^sys';
Find Privileges of a User on Objects
SELECT n.nspname as "Schema",
    c.relname as "Name",
    CASE c.relkind WHEN 'r' THEN 'table' WHEN 'v' THEN 'view' WHEN 'S' THEN 'sequence' WHEN 'f' THEN 'foreign table' END as "Type",
    pg_catalog.array_to_string(c.relacl, E'\n') AS "Access privileges",
    pg_catalog.array_to_string(ARRAY(
      SELECT attname || E':\n  ' || pg_catalog.array_to_string(attacl, E'\n  ')
      FROM pg_catalog.pg_attribute a
      WHERE attrelid = c.oid AND NOT attisdropped AND attacl IS NOT NULL
    ), E'\n') AS "Column access privileges"
  FROM pg_catalog.pg_class c
       LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
  WHERE c.relkind IN ('r', 'v', 'S', 'f')
    AND n.nspname !~ '^pg_' AND pg_catalog.pg_table_is_visible(c.oid) and pg_catalog.pg_get_userbyid(c.relowner)='owner'
  ORDER BY 1, 2;
Granting Privileges on All Procedures
select  'grant execute on procedure "CBF"."'||proname||'"('||pg_get_function_arguments(oid)||') to cbf_ctrl_user;' from pg_proc where pronamespace='     <oid of schema>'     ;

-- Get List of All Tables and Their Row Count
SELECT
pgClass.relname AS tableName,
pgClass.reltuples AS rowCount
FROM
pg_class pgClass
LEFT JOIN
pg_namespace pgNamespace ON (pgNamespace.oid = pgClass.relnamespace)
WHERE
pgNamespace.nspname NOT IN ('pg_catalog', 'information_schema') AND
pgClass.relkind='r';

-- Check Tables in Each User Defined Schema
SELECT n.nspname as "Schema",
  count(c.relname) as "Name"
FROM pg_catalog.pg_class c
     LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
WHERE c.relkind IN ('r',")
      AND n.nspname <> 'pg_catalog'
      AND n.nspname <> 'information_schema'
      AND n.nspname !~ '^pg_toast'
  AND pg_catalog.pg_table_is_visible(c.oid)
 group by n.nspname;
Find Parameters Changes for a Table
SELECT c.relname, pg_catalog.array_to_string(c.reloptions || array(select 'toast.' || x from pg_catalog.unnest(tc.reloptions) x), ', ')
FROM pg_catalog.pg_class c
 LEFT JOIN pg_catalog.pg_class tc ON (c.reltoastrelid = tc.oid)
WHERE c.relname = 'test'
Generate a Script to Change or Rename All Table Names to lower case
SELECT 'alter table "'||c.relname||'" rename to '||lower(c.relname)||';'
FROM pg_catalog.pg_class c
     LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
WHERE c.relkind ='r'
      AND n.nspname='schemaname'
ORDER BY 1;

-- Generate a Script to Change or Rename All Columns of a Table For Tables
SELECT
        'alter table "'||c.relname||'" rename "'||a.attname||'" to '||lower(a.attname)||';'
FROM
        pg_class c
        JOIN pg_attribute a ON a.attrelid = c.oid
        JOIN pg_type t ON a.atttypid = t.oid
        LEFT JOIN pg_catalog.pg_constraint r ON c.oid = r.conrelid
                AND r.conname = a.attname
WHERE
        c.relnamespace = (select oid from pg_namespace where nspname="schemaname")
        AND a.attnum > 0 AND c.relkind in ('r', 'p')
        AND c.relname = 'table_name'
ORDER BY a.attnum
For All Tables in a Schema
SELECT
        'alter      table "'||c.relname||'" rename "'||a.attname||'" to '||lower(a.attname)||';'
FROM
        pg_class c
        JOIN pg_attribute a ON a.attrelid = c.oid
        JOIN pg_type t ON a.atttypid = t.oid
        LEFT JOIN pg_catalog.pg_constraint r ON c.oid = r.conrelid
                AND r.conname = a.attname
WHERE
        c.relnamespace = (select oid from pg_namespace where nspname="schemaname")
        AND a.attnum > 0
        AND c.relkind in ('r', 'p')
ORDER BY a.attnum
Find Primary Keys on Tables of a Schema
SELECT c2.relname, i.indisprimary, i.indisunique, i.indisvalid, pg_catalog.pg_get_indexdef(i.indexrelid, 0, true),
  pg_catalog.pg_get_constraintdef(con.oid, true), contype
FROM pg_catalog.pg_class c, pg_catalog.pg_class c2, pg_catalog.pg_index i
  LEFT JOIN pg_catalog.pg_constraint con ON (conrelid = i.indrelid AND conindid = i.indexrelid AND contype IN ('p'))
WHERE c.relnamespace=(select oid from pg_namespace where nspname="public") AND c.oid = i.indrelid AND i.indexrelid = c2.oid
ORDER BY i.indisprimary DESC, i.indisunique DESC, c2.relname;
Find Sequences in a Schema
SELECT n.nspname as "Schema",
  c.relname as "Name",
  CASE c.relkind WHEN 'r' THEN 'table' WHEN 'v' THEN 'view' WHEN 'm' THEN 'materialized view' WHEN 'i' THEN 'index' WHEN 'S' THEN 'sequence' WHEN 's' THEN 'special' WHEN 'f' THEN 'foreign table' END as "Type",
  pg_catalog.pg_get_userbyid(c.relowner) as "Owner"
FROM pg_catalog.pg_class c
     LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
WHERE c.relkind IN ('S',”)
      AND n.nspname='schemaname'
ORDER BY 1,2;

-- Find the Constraints

SELECT r.conname
FROM pg_catalog.pg_constraint r
WHERE r.connamespace = (select oid from pg_namespace where nspname="public") AND r.contype = 'c'
ORDER BY 1;
Find ForeignKeys
SELECT conname,
  pg_catalog.pg_get_constraintdef(r.oid, true) as condef
FROM pg_catalog.pg_constraint r
WHERE r.connamespace=(select oid from pg_namespace where nspname="public") AND r.contype = 'f' ORDER BY 1;
Find Parent for ForeignKey
SELECT conname, conrelid::regclass, conindid::regclass,
  pg_catalog.pg_get_constraintdef(r.oid, true) as condef
FROM pg_catalog.pg_constraint r
WHERE r.connamespace=(select oid from pg_namespace where nspname="public") AND r.contype = 'f' ORDER BY 1;
Query to Find Sequence OWNED BY
select s.relname as "Sequence", n.nspname as "schema", t.relname as "Owned by table", a.attname as "Owned by column"
from pg_class s
  join pg_depend d on d.objid=s.oid and d.classid='pg_class'::regclass and d.refclassid='pg_class'::regclass
  join pg_class t on t.oid=d.refobjid
  join pg_namespace n on n.oid=t.relnamespace
  join pg_attribute a on a.attrelid=t.oid and a.attnum=d.refobjsubid
where s.relkind='S'
                                      
-- Real-Time Bloated Tables
select relname, n_live_tup, n_dead_tup, (n_dead_tup/(n_dead_tup+n_live_tup)::float)*100 as "% of bloat", last_autovacuum, 
last_autoanalyze from pg_stat_all_tables where (n_dead_tup+n_live_tup) > 0
and (n_dead_tup/(n_dead_tup+n_live_tup)::float)*100 > 0;
                                      
-- Tables That Are Being Updated the Most and Looking for VACUUM
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
