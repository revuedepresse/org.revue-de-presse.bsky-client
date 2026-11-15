:- module(link_expansion_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).

:- use_module('../../src/infrastructure/pg/connection', [
    close_pg_connection/1,
    open_pg_connection/1,
    pg_query_simple/3
]).
:- use_module('../../src/infrastructure/link/unfurl', [expand_text/3]).
:- use_module('../../src/infrastructure/repository/repository_publication_link', [
    expansion_of/3,
    attempts_since/3,
    record_expansion/4,
    record_pending/3,
    record_refusal/4
]).

/*
What ingestion does with a shortened link, end to end against the
containerised Postgres:

  1. a link nobody has resolved is left in the text and noted as pending,
     so the out-of-band resolver has a worklist;
  2. once the resolver has written the destination, the next post carrying
     that link is stored expanded;
  3. a link the service refused stays as the post wrote it, and is never
     asked about again.

Run with:
  make test-link-expansion
*/

probe("https://l.probe-expansion.test/aZ9").
other("https://l.probe-refused.test/Qk3").

expect(Label, Got, Expected) :-
    (   Got == Expected
    ->  format("[ok] ~s~n", [Label])
    ;   format("[KO] ~s~n  expected ~q~n  got      ~q~n", [Label, Expected, Got]), halt(1)
    ).

run_test :-
    open_pg_connection(C0),
    pg_query_simple(C0,
        "DELETE FROM publication_link WHERE short_url LIKE 'https://l.probe-%'",
        _),

    probe(Probe),
    other(Refused),

    % 1. Unknown: the text is untouched, and the link is on the worklist.
    append(["Une enquête 👉 ", Probe], PendingText),
    expand_text(pg_session(C0, C1), PendingText, Untouched),
    expect("an unresolved link is left exactly as the post wrote it", Untouched, PendingText),
    expansion_of(pg_session(C1, C2), short_url(Probe), AfterFirstPass),
    expect("and is recorded as pending for the resolver", AfterFirstPass, present("pending")),

    % 2. The resolver writes back; the next post is stored expanded.
    record_expansion(pg_session(C2, C3), short_url(Probe),
                     expanded("https://www.mediapart.fr/journal/france/enquete"), _),
    expand_text(pg_session(C3, C4), PendingText, Expanded),
    append(["Une enquête 👉 ", "https://www.mediapart.fr/journal/france/enquete"], ExpectedText),
    expect("the next post carrying it is stored expanded", Expanded, ExpectedText),

    % 3. A refusal is remembered, and changes nothing in the text.
    record_refusal(pg_session(C4, C5), short_url(Refused), http_status(403), _),
    append(["Voir ", Refused], RefusedText),
    expand_text(pg_session(C5, C6), RefusedText, StillShort),
    expect("a link the service would not serve stays short rather than breaking",
           StillShort, RefusedText),
    expansion_of(pg_session(C6, C7), short_url(Refused), RefusedRow),
    % A 403 says something about the asker, not about the link: it is kept
    % for a resolver asking from elsewhere, never written off.
    expect("a 403 is remembered as blocked, not as a verdict on the link",
           RefusedRow, present("blocked")),

    record_refusal(pg_session(C7, C7b), short_url("https://l.probe-gone.test/Ww1"),
                   http_status(404), _),
    expansion_of(pg_session(C7b, C7c), short_url("https://l.probe-gone.test/Ww1"), GoneRow),
    expect("a 404 is the one answer that is about the link", GoneRow, present("dead")),

    attempts_since(pg_session(C7c, C7d), minutes(60), Recent),
    (   Recent >= 3
    ->  format("[ok] every link asked about in the last hour is counted (~d), for the budget~n", [Recent])
    ;   format("[KO] the budget counter saw ~d attempts, expected at least 3~n", [Recent]), halt(1) ),

    % A post without any shortened link costs no query at all.
    expand_text(pg_session(C7d, C8), "rien à étendre ici", Plain),
    expect("a post with no shortened link comes back unchanged", Plain, "rien à étendre ici"),

    close_pg_connection(C8),

    % Fallback: with the ledger gone — not deployed yet, dropped, renamed —
    % collecting a post must still work, with its links as written.
    open_pg_connection(R0),
    pg_query_simple(R0, "ALTER TABLE publication_link RENAME TO publication_link_absent", _),
    close_pg_connection(R0),

    open_pg_connection(F0),
    expand_text(pg_session(F0, F1), PendingText, Survived),
    expect("with no ledger at all, the post keeps its text", Survived, PendingText),
    close_pg_connection(F1),

    open_pg_connection(R1),
    pg_query_simple(R1, "ALTER TABLE publication_link_absent RENAME TO publication_link", _),
    close_pg_connection(R1),

    format("[OK] link_expansion_test passed~n", []),
    halt(0).
