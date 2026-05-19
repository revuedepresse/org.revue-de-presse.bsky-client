-- Minimal status_popularity schema for repository_popularity testing.
-- Append-only table by design; no UNIQUE constraints.
-- The table is named status_popularity in production (legacy).

CREATE TABLE IF NOT EXISTS status_popularity (
    id               text PRIMARY KEY,
    status_id        text,
    publication_id   text,
    total_favorites  integer,
    total_retweets   integer,
    checked_at       timestamp
);
