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

links_of_day_sql(SQL) :-
    table(Table),
    append(
        [
            "SELECT DISTINCT match[1] AS url ",
            "FROM public.publication p ",
            "CROSS JOIN LATERAL regexp_matches( ",
            "  convert_from(decode(p.text, 'base64'), 'UTF8'), ",
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

rewrite_publication_sql(SQL) :-
    append(
        [
            "UPDATE public.publication ",
            "SET text = encode(convert_to(replace( ",
            "  convert_from(decode(text, 'base64'), 'UTF8'), $2, $3 ",
            "), 'UTF8'), 'base64') ",
            "WHERE published_at::date = $1::date ",
            "AND position($2 in convert_from(decode(text, 'base64'), 'UTF8')) > 0 ",
            "RETURNING 1"
        ],
        SQL
    ).

rewrite_status_sql(SQL) :-
    append(
        [
            "UPDATE public.weaving_status ",
            "SET ust_text = encode(convert_to(replace( ",
            "  convert_from(decode(ust_text, 'base64'), 'UTF8'), $2, $3 ",
            "), 'UTF8'), 'base64') ",
            "WHERE ust_created_at::date = $1::date ",
            "AND position($2 in convert_from(decode(ust_text, 'base64'), 'UTF8')) > 0"
        ],
        SQL
    ).
