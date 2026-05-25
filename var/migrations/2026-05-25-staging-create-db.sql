-- Staging database for the bsky-client stress test.
--
-- Mirrors the production org_revue_de_presse layout, isolated so a
-- full insert path (no pre-existing dedup hits) can be exercised
-- without touching production rows.
--
-- Run from a session NOT connected to org_revue_de_presse_staging
-- (e.g. from the default `postgres` database):
--
--   PGPASSWORD=$DATABASE_PASSWORD psql \
--     -h database.local -p 5434 \
--     -U io_marianne_caprica -d postgres \
--     -f var/migrations/2026-05-25-staging-create-db.sql
--
-- CREATE DATABASE cannot run inside a transaction block, hence the
-- separate file from the schema (2026-05-25-staging-schema.sql).

CREATE DATABASE org_revue_de_presse_staging
    OWNER io_marianne_caprica
    ENCODING 'UTF8'
    LC_COLLATE 'C'
    LC_CTYPE   'C'
    TEMPLATE   template0;

COMMENT ON DATABASE org_revue_de_presse_staging IS
    'Stress-test staging for org.revue-de-presse.bsky-client. '
    'Created 2026-05-25 to validate the pg_session refactor under '
    'full insert load (no dedup-hit short-circuit).';
