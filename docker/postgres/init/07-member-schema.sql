-- Minimal weaving_user schema for repository_list_item testing.
-- Columns mirror what repository_list_item:insert_record/3 writes and
-- record_by_screen_name/3 reads; types match production.

CREATE TABLE IF NOT EXISTS weaving_user (
    usr_id                       bigint PRIMARY KEY,
    usr_api_key                  varchar(255),
    max_status_id                varchar(255),
    min_status_id                varchar(255),
    max_like_id                  varchar(255),
    min_like_id                  varchar(255),
    total_statuses               integer,
    total_likes                  integer,
    description                  text,
    url                          text,
    last_status_publication_date timestamp,
    total_subscribees            integer,
    total_subscriptions          integer,
    usr_twitter_id               varchar(255),
    usr_twitter_username         varchar(255),
    usr_avatar                   varchar(255),
    usr_full_name                varchar(255),
    usr_status                   boolean NOT NULL,
    usr_user_name                varchar(255),
    usr_username_canonical       varchar(255),
    usr_email                    varchar(255),
    usr_email_canonical          varchar(255),
    protected                    boolean,
    suspended                    boolean,
    not_found                    boolean,
    usr_position_in_hierarchy    integer
);

-- Append-only log of profile snapshots written by
-- repository_list_item:insert/3.
CREATE TABLE IF NOT EXISTS member_profile_collected_event (
    id           text PRIMARY KEY,
    screen_name  text NOT NULL,
    payload      text,
    occurred_at  timestamptz NOT NULL,
    started_at   timestamptz NOT NULL,
    ended_at     timestamptz NOT NULL
);

-- One row per account and handle, as in production: a member that
-- changed handle gets a row under its current one.
CREATE UNIQUE INDEX IF NOT EXISTS unique_twitter_id_username
    ON weaving_user (usr_twitter_id, usr_twitter_username);
