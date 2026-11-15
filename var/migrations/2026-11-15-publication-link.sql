-- Ledger of shortened links and the addresses they lead to.
--
-- Ingestion stores the address a reader would land on rather than a
-- redirector's token (see src/infrastructure/link/unfurl.pl). It cannot
-- resolve a link itself: http_open/3 follows redirects inside the Rust
-- builtin, so the destination never reaches Prolog, and the services the
-- publishers use answer 403 to every non-browser client. So the collector
-- reads this table, applies what is known, and records what is not as
-- `pending` -- a worklist for the out-of-band resolver, which drives a real
-- browser and writes its findings back here (make import-link-expansions).
--
-- One row per shortened link, never per publication: the same token is
-- shared by every post that carries it, and is asked for at most once.
--
-- status:
--   pending  -- seen in a post, destination unknown
--   resolved -- expanded_url is where it leads
--   refused  -- the service answered 4xx to us; do not keep asking
--   dead     -- the service answered 404/410; the link leads nowhere

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
