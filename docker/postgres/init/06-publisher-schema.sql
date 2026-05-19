-- Minimal publishers_list schema for repository_publisher testing.
-- id and list_id are bigint -> exercises the $1::bigint / $2::bigint
-- bind casts in repository_publisher:insert/2.

CREATE TABLE IF NOT EXISTS publishers_list (
    id              bigint PRIMARY KEY,
    list_id         bigint NOT NULL,
    public_id       text NOT NULL,
    name            text NOT NULL,
    screen_name     text NOT NULL,
    locked          boolean NOT NULL DEFAULT false,
    created_at      timestamptz NOT NULL DEFAULT NOW(),
    total_members   integer,
    total_statuses  integer,
    UNIQUE (name, screen_name)
);
