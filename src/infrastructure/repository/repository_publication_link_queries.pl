:- module(repository_publication_link_queries, [
    attempts_sql/1,
    expansion_sql/1,
    links_of_day_sql/1,
    lookup_sql/1,
    pending_sql/1,
    refusal_sql/1,
    rewrite_publication_sql/1,
    rewrite_status_sql/1
]).

:- use_module(library(lists)).

/**
The statements `repository_publication_link` sends, kept apart from the
sending.

Each predicate here relates a statement to the chars it is made of, by
append/2 over literals: no session, no connection, nothing that waits on
Postgres. That is the whole reason they sit apart -- a prover loads this
file on its own, whereas `repository_publication_link` cannot be loaded
without the wire client, and the client cannot be loaded without sockets, so
the shape of a statement stayed out of LPTP's reach for a reason that was
never its own.

The table name sits here, beside the statements that spell it, and is not
exported: a name read across a module boundary looks like state when it is
nothing but a word in the SQL. Should `repository_publication_link` come to
need it again, saying the line twice is the cheaper mistake.
*/

table("publication_link").

lookup_sql(SQL) :-
    table(Table),
    append(
        [
            "SELECT status, coalesce(expanded_url, '') ",
            "FROM public.", Table, " ",
            "WHERE short_url = $1 ",
            "LIMIT 1"
        ],
        SQL
    ).

pending_sql(SQL) :-
    table(Table),
    append(
        [
            "INSERT INTO public.", Table, " ",
            "(short_url, status, attempts, first_seen_at, last_attempt_at) ",
            "VALUES ($1, 'pending', 0, now()::text, NULL) ",
            "ON CONFLICT (short_url) DO NOTHING"
        ],
        SQL
    ).

expansion_sql(SQL) :-
    table(Table),
    append(
        [
            "INSERT INTO public.", Table, " ",
            "(short_url, expanded_url, status, http_status, attempts, first_seen_at, last_attempt_at) ",
            "VALUES ($1, $2, 'resolved', 200, 1, now()::text, now()::text) ",
            "ON CONFLICT (short_url) DO UPDATE SET ",
            "expanded_url = EXCLUDED.expanded_url, ",
            "status = 'resolved', ",
            "http_status = EXCLUDED.http_status, ",
            "attempts = public.", Table, ".attempts + 1, ",
            "last_attempt_at = EXCLUDED.last_attempt_at"
        ],
        SQL
    ).

refusal_sql(SQL) :-
    table(Table),
    append(
        [
            "INSERT INTO public.", Table, " ",
            "(short_url, status, http_status, attempts, first_seen_at, last_attempt_at) ",
            "VALUES ($1, $2, $3::integer, 1, now()::text, now()::text) ",
            "ON CONFLICT (short_url) DO UPDATE SET ",
            "status = EXCLUDED.status, ",
            "http_status = EXCLUDED.http_status, ",
            "attempts = public.", Table, ".attempts + 1, ",
            "last_attempt_at = EXCLUDED.last_attempt_at"
        ],
        SQL
    ).

attempts_sql(SQL) :-
    table(Table),
    append(
        [
            "SELECT count(*)::text ",
            "FROM public.", Table, " ",
            "WHERE last_attempt_at IS NOT NULL ",
            "AND last_attempt_at::timestamptz > now() - ($1 || ' minutes')::interval"
        ],
        SQL
    ).

%% decoded_column(+Column, -SQL)
%
% The text of a column that may or may not be base64.
%
% `publication.text` and `weaving_status.ust_text` hold two generations of
% rows: the legacy ones are plain text (publications up to 22 July 2023,
% and the bulk of weaving_status), while everything the worker has written
% is base64 of the quoted term. `decode(Plain, 'base64')` does not return
% something odd on a plain row, it raises, and SQL gives no promise about
% the order two conditions in a WHERE are evaluated in — so a guard sitting
% beside the decode is not a guard at all. CASE is the one construct whose
% evaluation order is defined, which is why the test lives inside it.
%% base64_column(+Column, -SQL)
%
% The test that tells the two generations of row apart. Named so that the
% decode and the re-encode cannot drift into disagreeing about which rows
% are which.
%
% The alphabet alone is not a test: a one-word post with no punctuation is
% spelled entirely in base64 characters, passes it, and decodes to bytes
% `convert_from` rejects -- which raises, taking the whole day's rewrite
% with it. Two further conditions narrow it to what the encoder actually
% writes: base64 of a quoted term is a whole number of quartets, and begins
% with the quartet for `"` or for `[`, which is to say `I` or `W`.
%
% Newlines belong in the alphabet because `encode(..., 'base64')` wraps at
% 76 characters, and rows written before that wrap was stripped carry them.
% Leaving them out read those rows as plain text and skipped them, which is
% the one case where skipping is not harmless: `chars_base64/3` fails on a
% newline, so a wrapped row is one the worker cannot read until it is
% rewritten unwrapped.
%
% What remains uncovered: a post that is a single base64-spelled token, of a
% length divisible by four, beginning with `I` or `W`, whose bytes are not
% valid UTF-8. Closing that needs a decode that returns rather than raises,
% which in Postgres means a function, which means a migration.
base64_column(Column, SQL) :-
    append(
        [
            "(", Column, " ~ E'^[A-Za-z0-9+/=\\n]+$' ",
            "AND length(replace(", Column, ", chr(10), '')) % 4 = 0 ",
            "AND left(", Column, ", 1) IN ('I', 'W'))"
        ],
        SQL
    ).

decoded_column(Column, SQL) :-
    base64_column(Column, Test),
    append(
        [
            "CASE WHEN ", Test, " ",
            "THEN convert_from(decode(replace(", Column, ", chr(10), ''), 'base64'), 'UTF8') ",
            "ELSE ", Column, " END"
        ],
        SQL
    ).

links_of_day_sql(SQL) :-
    table(Table),
    decoded_column("p.text", Decoded),
    append(
        [
            "SELECT DISTINCT match[1] AS url ",
            "FROM public.publication p ",
            "CROSS JOIN LATERAL regexp_matches( ",
            "  ", Decoded, ", ",
            "  'https?://[^\\s\"]+', 'g' ",
            ") AS match ",
            "WHERE p.published_at::date = $1::date ",
            "AND NOT EXISTS ( ",
            "  SELECT 1 FROM public.", Table, " l ",
            "  WHERE l.short_url = match[1] ",
            "  AND l.status IN ('resolved', 'dead') ",
            ") ",
            "ORDER BY 1"
        ],
        SQL
    ).

%% rewrite_publication_sql(-SQL)
%
% Re-encoding is wrapped in `replace(..., chr(10), '')` because Postgres
% `encode(..., 'base64')` wraps its output every 76 characters, and the
% reader on the other side is `chars_base64/3`, which fails outright on a
% newline. Without that strip, a rewritten row is a row the worker can no
% longer read.
rewrite_publication_sql(SQL) :-
    decoded_column("text", Decoded),
    base64_column("text", Encoded),
    append(
        [
            "UPDATE public.publication ",
            "SET text = CASE WHEN ", Encoded, " ",
            "THEN replace(encode(convert_to(replace( ",
            "  ", Decoded, ", $2, $3 ",
            "), 'UTF8'), 'base64'), chr(10), '') ",
            "ELSE replace(text, $2, $3) END ",
            "WHERE published_at::date = $1::date ",
            "AND position($2 in ", Decoded, ") > 0 ",
            "RETURNING 1"
        ],
        SQL
    ).

rewrite_status_sql(SQL) :-
    decoded_column("ust_text", Decoded),
    base64_column("ust_text", Encoded),
    append(
        [
            "UPDATE public.weaving_status ",
            "SET ust_text = CASE WHEN ", Encoded, " ",
            "THEN replace(encode(convert_to(replace( ",
            "  ", Decoded, ", $2, $3 ",
            "), 'UTF8'), 'base64'), chr(10), '') ",
            "ELSE replace(ust_text, $2, $3) END ",
            "WHERE ust_created_at::date = $1::date ",
            "AND position($2 in ", Decoded, ") > 0"
        ],
        SQL
    ).
