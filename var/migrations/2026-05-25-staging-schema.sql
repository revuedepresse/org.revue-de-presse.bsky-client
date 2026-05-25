-- Staging schema for org_revue_de_presse_staging.
--
-- Mirrors the production column types + constraints for the three
-- tables the bsky-client worker writes to (weaving_status,
-- publication, status_popularity) so the insert path exercises the
-- same on-the-wire bind shapes as production.
--
-- Run after var/migrations/2026-05-25-staging-create-db.sql, from a
-- session connected to org_revue_de_presse_staging:
--
--   PGPASSWORD=$DATABASE_PASSWORD psql \
--     -h database.local -p 5434 \
--     -U io_marianne_caprica -d org_revue_de_presse_staging \
--     -v ON_ERROR_STOP=1 \
--     -f var/migrations/2026-05-25-staging-schema.sql

BEGIN;

-- weaving_status: status-keyed pipeline table. Mirrors the production
-- column types from `\d public.weaving_status`.
CREATE TABLE IF NOT EXISTS weaving_status (
    ust_id            bigserial PRIMARY KEY,
    ust_hash          character varying(255),
    ust_name          text                            NOT NULL,
    ust_full_name     character varying(32)           NOT NULL,
    ust_text          text                            NOT NULL,
    ust_avatar        character varying(255)          NOT NULL,
    ust_access_token  character varying(255)          NOT NULL,
    ust_status_id     character varying(255),
    ust_api_document  text,
    ust_starred       boolean                         NOT NULL DEFAULT false,
    ust_indexed       boolean                         NOT NULL DEFAULT false,
    ust_created_at    timestamp(0) without time zone  NOT NULL,
    ust_updated_at    timestamp(0) without time zone,
    is_published      boolean                         NOT NULL DEFAULT false
);

CREATE UNIQUE INDEX IF NOT EXISTS status_unique_hash
    ON weaving_status (ust_hash);
ALTER TABLE weaving_status
    ADD CONSTRAINT weaving_status_ust_hash_unique UNIQUE USING INDEX status_unique_hash;

CREATE INDEX IF NOT EXISTS idx_published     ON weaving_status (is_published);
CREATE INDEX IF NOT EXISTS indexed_idx       ON weaving_status (ust_indexed);
CREATE INDEX IF NOT EXISTS status_id_idx     ON weaving_status (ust_status_id);
CREATE INDEX IF NOT EXISTS status_screen_name ON weaving_status (ust_full_name);
CREATE INDEX IF NOT EXISTS ust_created_at    ON weaving_status (ust_created_at);

-- publication: per-post canonical record. INSERT ... ON CONFLICT (hash)
-- DO NOTHING RETURNING legacy_id is the hot path; UNIQUE constraint
-- below is what makes it idempotent.
CREATE TABLE IF NOT EXISTS publication (
    id            text   PRIMARY KEY,
    legacy_id     bigint,
    hash          text   NOT NULL,
    screen_name   text,
    text          text,
    avatar_url    text,
    document_id   text,
    document      text,
    published_at  text
);

ALTER TABLE publication
    ADD CONSTRAINT publication_hash_unique UNIQUE (hash);

-- status_popularity: append-only popularity snapshots. No UNIQUE
-- constraint by design; downstream readers reduce by publication_id.
CREATE TABLE IF NOT EXISTS status_popularity (
    id               text       PRIMARY KEY,
    status_id        text,
    publication_id   text,
    total_favorites  integer,
    total_retweets   integer,
    checked_at       timestamp
);

CREATE INDEX IF NOT EXISTS status_popularity_publication_id_idx
    ON status_popularity (publication_id);

-- scram_probe: minimal target for the SCRAM-SHA-256 handshake test
-- the wire client runs on connect.
CREATE TABLE IF NOT EXISTS scram_probe (
    id    integer PRIMARY KEY,
    note  text    NOT NULL
);
INSERT INTO scram_probe (id, note) VALUES (1, 'scram-sha-256 ok')
ON CONFLICT (id) DO NOTHING;

COMMIT;

-- Sanity check: every table should be present and empty.
SELECT 'weaving_status'    AS table_name, count(*) FROM weaving_status
UNION ALL
SELECT 'publication'       AS table_name, count(*) FROM publication
UNION ALL
SELECT 'status_popularity' AS table_name, count(*) FROM status_popularity
UNION ALL
SELECT 'scram_probe'       AS table_name, count(*) FROM scram_probe;
