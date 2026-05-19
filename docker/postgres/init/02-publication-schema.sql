-- Minimal publication schema for the bind-param + idempotence tests.
-- Mirrors the production columns the repository writes; types kept loose
-- (text) because we exercise the I/O path, not column constraints.

CREATE TABLE IF NOT EXISTS publication (
    id            text PRIMARY KEY,
    legacy_id     bigint,
    hash          text NOT NULL,
    screen_name   text,
    text          text,
    avatar_url    text,
    document_id   text,
    document      text,
    published_at  text
);

ALTER TABLE publication
    ADD CONSTRAINT publication_hash_unique UNIQUE (hash);
