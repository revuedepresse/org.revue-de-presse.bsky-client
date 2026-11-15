-- Ledger of shortened links, mirrored from
-- var/migrations/2026-11-15-publication-link.sql so the containerised
-- Postgres the tests run against carries the same table as production.

CREATE TABLE IF NOT EXISTS publication_link (
    short_url        text PRIMARY KEY,
    expanded_url     text,
    status           text    NOT NULL DEFAULT 'pending',
    http_status      integer,
    attempts         integer NOT NULL DEFAULT 0,
    first_seen_at    text,
    last_attempt_at  text
);

CREATE INDEX IF NOT EXISTS publication_link_status_idx
    ON publication_link (status);
