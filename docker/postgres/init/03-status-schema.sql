-- Minimal weaving_status schema for repository_status:insert/3 testing.
-- The table is named weaving_status in production (legacy); the
-- repository module exposes it as the conceptual "status" entity.
-- ust_* column names mirror production.

CREATE TABLE IF NOT EXISTS weaving_status (
    ust_id            bigserial PRIMARY KEY,
    ust_hash          text NOT NULL,
    ust_name          text,
    ust_full_name     text,
    ust_text          text,
    ust_avatar        text,
    ust_api_document  text,
    ust_status_id     text,
    ust_access_token  text,
    is_published      boolean,
    ust_created_at    text
);

ALTER TABLE weaving_status
    ADD CONSTRAINT weaving_status_ust_hash_unique UNIQUE (ust_hash);
