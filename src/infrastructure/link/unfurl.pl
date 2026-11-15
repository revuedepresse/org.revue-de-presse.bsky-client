:- module(unfurl, [
    expand_text/3
]).

:- use_module(library(dif)).
:- use_module(library(reif)).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).

:- use_module('../repository/repository_publication_link', [
    expansion_of/3,
    record_expansion/4,
    record_pending/3,
    record_refusal/4
]).
:- use_module('../../link_shortener', [
    page_destination/2,
    permalink/2,
    rewrite_urls/3,
    shortened_urls_in_text/2
]).
:- use_module('../../configuration', [link_expansion/1]).
:- use_module('../../stream', [
    read_stream/2,
    writeln/2
]).

/**
Replaces the shortened links in a post with the addresses they lead to,
at the moment the post is collected.

A link the ledger has never seen is asked for once, by us, under our own
name. `http_open/3` follows the redirects inside the Rust builtin and
never hands back the address it arrived at, so the destination is read
from what the page claims for itself — `<link rel="canonical">`, else
`<meta property="og:url">` — and reduced to a permalink before it is
stored.

The redirectors most French publishers use — `l.mediapart.fr`,
`l.humanite.fr`, `l.reporterre.net`, `www.altereco.media` and some twenty
more, all one provider — refuse curl and a headless browser with a `403`
block page, but answer an identified client with a `200` and an
interstitial that bounces through JavaScript. That page carries the
`<noscript>` fallback a client without JavaScript is meant to follow, and
the destination is in it. Services that redirect outright
(`l.franceinfo.fr`, `f.mtr.cool`, `youtu.be`, `bit.ly`, `tinyurl.com`)
are followed by http_open/3 and claim themselves through their canonical
link.

A service that does refuse is recorded `refused` on that single attempt
and never asked again; a resolver driving a real browser can answer those
out of band and write back to this same table.

One request per distinct link, ever: a link already resolved, refused,
dead or pending is never fetched again, whatever number of posts carry
it.

A link the ledger has refused or declared dead is left exactly as the post
wrote it: a reader still gets a working link, and no service is asked twice.
*/

%% expand_text(+Session, +Text, -Expanded)
%
% Threads the session: the ledger is read once per distinct shortened link
% in the post, and written once for each one never seen before.
expand_text(pg_session(C0, C), Text, Expanded) :-
    link_expansion(Setting),
    expand_text_(Setting, pg_session(C0, C), Text, Expanded).

%% expand_text_(+Setting, +Session, +Text, -Expanded)
%
% Switched off, or failing for any reason — a ledger table that is not
% there yet, a query that errors, a redirector that answers something
% unreadable — the post is stored exactly as written. Collecting a post
% is the job; expanding its links is an improvement on it, and an
% improvement must never be the reason a post is lost.
expand_text_("off", pg_session(C, C), Text, Text).
expand_text_("on", pg_session(C0, C), Text, Expanded) :-
    catch(
        expand_known(pg_session(C0, C), Text, Expanded),
        Error,
        ( writeln([link_expansion_unavailable|[Error]], true),
          ensure_bound(pg_session(C0, C)),
          Expanded = Text )
    ).

%% ensure_bound(+Session)
%
% The session's output slot may still be unbound when a DB call threw
% mid-chain; binding it to the input leaves the caller with a usable
% connection and no DB state changed. Two clauses gated on var/1, as
% event_getAuthorFeed.pl does for the same reason.
ensure_bound(pg_session(In, Out)) :-
    var(Out),
    Out = In.
ensure_bound(pg_session(_, Out)) :-
    nonvar(Out).

expand_known(pg_session(C0, C), Text, Expanded) :-
    shortened_urls_in_text(Text, Shortened),
    expansion_pairs(pg_session(C0, C), Shortened, Pairs),
    rewrite_urls(Text, Pairs, Expanded).

expansion_pairs(pg_session(C, C), [], []).
expansion_pairs(pg_session(C0, C), [Url|Urls], Pairs) :-
    expansion_of(pg_session(C0, C1), short_url(Url), Found),
    pairs_for(Found, pg_session(C1, C2), Url, Pairs, Rest),
    expansion_pairs(pg_session(C2, C), Urls, Rest).

%% pairs_for(+Found, +Session, +Url, -Pairs, -Rest)
%
% Known destination: the pair goes in front of whatever the other links
% contribute. Known but unresolved, or refused, or dead: nothing to
% substitute. Never seen: noted for the resolver, and nothing to
% substitute yet.
pairs_for(present(expanded(Expanded)), pg_session(C, C), Url, [Url-Expanded|Rest], Rest).
pairs_for(present(Status), pg_session(C, C), _, Rest, Rest) :-
    dif(Status, expanded(_)).
pairs_for(absent, pg_session(C0, C), Url, Pairs, Rest) :-
    resolve_remote(Url, Outcome),
    remember(Outcome, pg_session(C0, C), Url),
    substitution(Outcome, Url, Pairs, Rest).

%% substitution(+Outcome, +Url, -Pairs, -Rest)
substitution(resolved(Expanded), Url, [Url-Expanded|Rest], Rest).
substitution(refused(_), _, Rest, Rest).
substitution(unavailable(_), _, Rest, Rest).

%% remember(+Outcome, +Session, +Url)
%
% A refusal is final: the service answered, and not with a destination.
% An unavailable answer — a timeout, a name that does not resolve, a page
% claiming no address of its own — says nothing about the link, so it
% stays pending for the browser resolver rather than being written off.
remember(resolved(Expanded), pg_session(C0, C), Url) :-
    record_expansion(pg_session(C0, C), short_url(Url), expanded(Expanded), _).
remember(refused(Code), pg_session(C0, C), Url) :-
    record_refusal(pg_session(C0, C), short_url(Url), http_status(Code), _).
remember(unavailable(Reason), pg_session(C0, C), Url) :-
    writeln([link_unresolved|[Url, Reason]], true),
    record_pending(pg_session(C0, C), short_url(Url), _).

%% resolve_remote(+Url, -Outcome)
%
% One GET, under our own name. Any exception — a name that does not
% resolve, a connection refused, a body that ends mid-stream — becomes
% `unavailable`, never a failure of the ingest: a post is worth storing
% whether or not its links could be followed.
resolve_remote(Url, Outcome) :-
    catch(attempt(Url, Outcome), Error, Outcome = unavailable(Error)).

%% attempt(+Url, -Outcome)
%
% `once/1` guards the foreign call alone: http_open/3 can fail outright —
% a socket the runtime declines to open — and a post is worth storing
% whether or not its links could be followed.
attempt(Url, Outcome) :-
    once(( fetch(Url, Outcome)
         ; Outcome = unavailable(no_answer)
         )).

fetch(Url, Outcome) :-
    user_agent(UserAgent),
    http_open(Url, Stream, [
        status_code(Code),
        request_headers([UserAgent])
    ]),
    read_stream(Stream, Body),
    outcome_of(Code, Url, Body, Outcome).

outcome_of(200, Url, Body, Outcome) :-
    page_destination(Body, Found),
    destination_outcome(Found, Url, Outcome).
outcome_of(Code, _, _, refused(Code)) :-
    dif(Code, 200).

%% destination_outcome(+Found, +Url, -Outcome)
%
% A page that claims no address of its own, or claims the one just asked
% for, has not resolved anything.
destination_outcome(none, _, unavailable(no_destination_in_page)).
destination_outcome(destination(Claimed), Url, Outcome) :-
    permalink(Claimed, Expanded),
    resolved_or_same(Expanded, Url, Outcome).

resolved_or_same(Expanded, Url, Outcome) :-
    if_(Expanded = Url,
        Outcome = unavailable(no_destination_in_page),
        Outcome = resolved(Expanded)).

%% user_agent(-Header)
%
% Named, with an address to complain to: a service that would rather not
% be read by a program can say so, and that refusal is recorded rather
% than worked around.
user_agent('user-agent'("revue-de-presse-link-expander (+https://revue-de-presse.org)")).
