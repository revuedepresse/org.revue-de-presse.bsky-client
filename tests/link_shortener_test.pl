:- module(link_shortener_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).

:- use_module('../src/link_shortener', [
    canonical_url/2,
    permalink/2,
    rewrite_urls/3,
    shortened_url/1,
    shortened_urls_in_text/2,
    urls_in_text/2
]).

/*
Pins src/link_shortener.pl against the addresses actually found in the
ranked publications: 3,329 shortened links over 34 hosts, next to article
addresses that must be left alone.

Run with:
  scryer-prolog ./tests/link_shortener_test.pl -g run_test

DB-free and HTTP-free.
*/

shortened([
    "https://l.mediapart.fr/MWq",
    "https://l.reporterre.net/01H",
    "https://l.humanite.fr/0B3",
    "https://go.france24.com/7Sip",
    "https://www.altereco.media/0BF",
    "https://bit.ly/4iLEDhE",
    "https://buff.ly/IMiSQzD",
    "https://tinyurl.com/32f5hpnf",
    "https://youtu.be/2OULQjal49c",
    "https://f.mtr.cool/3ws9xjtp1f",
    "http://l.vert.eco/2V"
]).

% Addresses a reader would recognise: nothing here may be touched.
not_shortened([
    "https://reporterre.net/Le-carburant-des-avions-est-mieux-rembourse",
    "https://www.youtube.com/watch?v=NqpTCOz1A3w",
    "https://madeinperpignan.com/abandonnes-ou-a-peine-habites-ces-villages",
    "https://www.humanite.fr/medias/algerie/jean-michel-aphatie-mis-en-retrait",
    "https://revue-de-presse.org/",
    "https://www.coop-medias.org/les-medias-independants/"
]).

check_shortened([]).
check_shortened([U|Us]) :-
    (   shortened_url(U)
    ->  true
    ;   format("[KO] ~s was not recognised as shortened~n", [U]), halt(1)
    ),
    check_shortened(Us).

check_not_shortened([]).
check_not_shortened([U|Us]) :-
    (   shortened_url(U)
    ->  format("[KO] ~s was taken for a shortened link~n", [U]), halt(1)
    ;   true
    ),
    check_not_shortened(Us).

expect(Label, Got, Expected) :-
    (   Got == Expected
    ->  format("[ok] ~s~n", [Label])
    ;   format("[KO] ~s~n  expected ~q~n  got      ~q~n", [Label, Expected, Got]), halt(1)
    ).

run_test :-
    shortened(Shortened),
    check_shortened(Shortened),
    format("[ok] every redirector shape is recognised (~d)~n", [11]),

    not_shortened(Plain),
    check_not_shortened(Plain),
    format("[ok] article addresses are left alone (~d)~n", [6]),

    % A sentence leaves punctuation against the link; the address stops there.
    urls_in_text(
        "Pour lire notre enquête 👉 https://l.mediapart.fr/MWq, et aussi https://l.vert.eco/2V.",
        Found
    ),
    expect("the two addresses are read without the comma or full stop", Found,
           ["https://l.mediapart.fr/MWq", "https://l.vert.eco/2V"]),

    % A link repeated in one post must be asked for once.
    shortened_urls_in_text(
        "https://l.humanite.fr/0B3 … à lire : https://l.humanite.fr/0B3 (https://www.humanite.fr/accueil)",
        ToResolve
    ),
    expect("a repeated link is listed once, the plain one not at all", ToResolve,
           ["https://l.humanite.fr/0B3"]),

    rewrite_urls(
        "Enquête 👉 https://l.mediapart.fr/MWq.",
        ["https://l.mediapart.fr/MWq"-"https://www.mediapart.fr/journal/france/enquete"],
        Rewritten
    ),
    expect("the expansion takes the place of the token, punctuation kept", Rewritten,
           "Enquête 👉 https://www.mediapart.fr/journal/france/enquete."),

    rewrite_urls("rien à faire ici", [], Untouched),
    expect("a post without links comes back unchanged", Untouched, "rien à faire ici"),

    % What the collector does when a service refuses: it knows one of the two.
    rewrite_urls(
        "a https://l.vert.eco/2V b https://l.marsactu.fr/ly8",
        ["https://l.vert.eco/2V"-"https://vert.eco/articles/le-sujet"],
        Partial
    ),
    expect("an unresolved link stays as it is", Partial,
           "a https://vert.eco/articles/le-sujet b https://l.marsactu.fr/ly8"),

    % What a page claims for itself, whichever order its attributes come in.
    canonical_url(
        "<head><meta charset=\"utf-8\"><link rel=\"canonical\" href=\"https://www.humanite.fr/medias/algerie/jean-michel-aphatie\"></head>",
        FromLink
    ),
    expect("the canonical link is read", FromLink,
           "https://www.humanite.fr/medias/algerie/jean-michel-aphatie"),

    canonical_url(
        "<head><link href=\"https://reporterre.net/Le-carburant\" rel=\"canonical\"/></head>",
        HrefFirst
    ),
    expect("href before rel is read just the same", HrefFirst,
           "https://reporterre.net/Le-carburant"),

    canonical_url(
        "<head><meta property=\"og:url\" content=\"https://vert.eco/articles/le-sujet\"></head>",
        FromMeta
    ),
    expect("og:url answers when there is no canonical link", FromMeta,
           "https://vert.eco/articles/le-sujet"),

    (   canonical_url("<head><title>rien</title></head>", _)
    ->  format("[KO] a page claiming no address must not yield one~n", []), halt(1)
    ;   format("[ok] a page claiming no address yields nothing~n", []) ),

    % The campaign belongs to the redirect, not to the article.
    permalink(
        "https://www.radiofrance.fr/dossiers/effet-matilda?at_campaign=bsky&at_medium=social&utm_source=x",
        Stripped
    ),
    expect("campaign parameters are dropped", Stripped,
           "https://www.radiofrance.fr/dossiers/effet-matilda"),

    permalink("https://www.youtube.com/watch?v=NqpTCOz1A3w&feature=youtu.be", KeptV),
    expect("a parameter the address needs is kept", KeptV,
           "https://www.youtube.com/watch?v=NqpTCOz1A3w"),

    permalink("https://reporterre.net/Le-carburant", NoQuery),
    expect("an address without a query is returned as it is", NoQuery,
           "https://reporterre.net/Le-carburant"),

    format("[OK] link_shortener_test passed~n", []),
    halt(0).
