-- Minimal publishers_list_collected_event schema for repository_list testing.
-- Columns mirror what repository_list:insert/2 writes; types match the
-- production casts (list_id bigint -> exercises the $2::bigint bind cast).

CREATE TABLE IF NOT EXISTS publishers_list_collected_event (
    id           text PRIMARY KEY,
    list_id      bigint NOT NULL,
    list_name    text NOT NULL,
    payload      text,
    occurred_at  timestamptz NOT NULL,
    started_at   timestamptz NOT NULL,
    ended_at     timestamptz NOT NULL
);
