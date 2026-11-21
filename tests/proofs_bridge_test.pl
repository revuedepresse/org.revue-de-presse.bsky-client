:- module(proofs_bridge_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).

:- use_module('../src/link_expansion', [
    answer_of_outcome/2,
    expansion_decisions/4,
    outcome_of/4,
    plan_of/2,
    refusal_status/2
]).
:- use_module('../src/link_shortener', [
    destination_url/2,
    permalink/2,
    rewrite_urls/3,
    shortened_url_t/2,
    trim_trailing/2,
    shortened_urls_in_text/2,
    urls_in_text/2
]).

% The mirror is plain Prolog with no module directive — compile_gr/1 reads it
% that way — so it is consulted at run time and its predicates are called in
% `user`, rather than imported.

/*
The obligation every proof beside it rests on.

proofs/kernel/recogniser.pl is a mirror: LPTP reads definite clauses and
negation of ground goals, and nothing else, so the shipped recogniser —
written with if_/3, memberd_t/3, tfilter/3, zcompare/3 — is restated there
in the fragment LPTP does read, with characters as their codes. Every
theorem in recogniser.pr is therefore about the mirror. Without this test,
"permalink/2 is idempotent" would be a statement about a file nothing
ships.

So: run both over the same inputs and compare. The mirror speaks codes and
the shipped module speaks characters, which is the one translation allowed
here; anything else differing is a mirror that has drifted from what it
mirrors, and the proofs stop meaning anything about the collector.

The corpus is the addresses the rankings actually carry — the redirectors
of twenty-odd French publishers, the article addresses they lead to, the
campaign parameters they arrive with — plus the shapes the tests of
src/link_shortener.pl already pin.

Run with:
  make test-proofs-bridge
*/

codes_of(Chars, Codes) :-
    maplist(char_code_of, Chars, Codes).

char_code_of(Char, Code) :-
    char_code(Char, Code).

corpus_url("https://l.mediapart.fr/MWq").
corpus_url("https://l.humanite.fr/AXQ").
corpus_url("https://l.reporterre.net/01H").
corpus_url("https://go.france24.com/7Sip").
corpus_url("https://www.altereco.media/0BF").
corpus_url("https://bit.ly/4iLEDhE").
corpus_url("https://youtu.be/2OULQjal49c").
corpus_url("http://l.vert.eco/2V").
corpus_url("https://reporterre.net/Le-carburant-des-avions").
corpus_url("https://www.youtube.com/watch?v=NqpTCOz1A3w&feature=youtu.be").
corpus_url("https://www.radiofrance.fr/dossiers/effet-matilda?at_campaign=bsky&at_medium=social").
corpus_url("https://www.humanite.fr/medias/algerie/jean-michel-aphatie?utm_source=x&utm_medium=social").
corpus_url("https://www.coop-medias.org/").
corpus_url("https://revue-de-presse.org/?liste=coop-medias").

corpus_text("Pour lire notre enquête 👉 https://l.mediapart.fr/MWq, et aussi https://l.vert.eco/2V.").
corpus_text("https://l.humanite.fr/0B3 … à lire : https://l.humanite.fr/0B3 (https://www.humanite.fr/accueil)").
corpus_text("rien à étendre ici").
corpus_text("Voir https://bit.ly/4iLEDhE »").

corpus_html("<head><meta charset=\"utf-8\"><link rel=\"canonical\" href=\"https://www.humanite.fr/medias/algerie\"></head>").
corpus_html("<head><meta http-equiv=\"refresh\" content=\"0;url=https://www.humanite.fr/medias/algerie\"><link rel=\"canonical\" href=\"https://l.humanite.fr/AXQ\"></head>").
corpus_html("<head><meta property=\"og:url\" content=\"https://vert.eco/articles/le-sujet\"></head>").
corpus_html("<head><title>rien</title></head>").

%% agree_chars(+Label, +Shipped, +MirrorCodes)
agree_chars(Label, Shipped, MirrorCodes) :-
    codes_of(Shipped, ShippedCodes),
    (   ShippedCodes == MirrorCodes
    ->  true
    ;   format("[KO] ~s~n  shipped ~q~n  mirror  ~q~n", [Label, ShippedCodes, MirrorCodes]),
        halt(1)
    ).

check_permalink([]).
check_permalink([Url|Urls]) :-
    permalink(Url, Shipped),
    codes_of(Url, UrlCodes),
    (   user:r_permalink(UrlCodes, MirrorCodes)
    ->  agree_chars("permalink/2 and r_permalink/2 disagree", Shipped, MirrorCodes)
    ;   format("[KO] r_permalink/2 found no answer for ~s~n", [Url]), halt(1) ),
    check_permalink(Urls).

check_shortened([]).
check_shortened([Url|Urls]) :-
    shortened_url_t(Url, Truth),
    codes_of(Url, UrlCodes),
    (   user:r_shortened_url_t(UrlCodes, MirrorTruth)
    ->  true
    ;   format("[KO] r_shortened_url_t/2 found no answer for ~s~n", [Url]), halt(1) ),
    (   Truth == MirrorTruth
    ->  true
    ;   format("[KO] ~s: shipped says ~w, mirror says ~w~n", [Url, Truth, MirrorTruth]), halt(1) ),
    check_shortened(Urls).

check_text([]).
check_text([Text|Texts]) :-
    urls_in_text(Text, Shipped),
    codes_of(Text, TextCodes),
    (   user:r_urls_in_text(TextCodes, MirrorUrls)
    ->  true
    ;   format("[KO] r_urls_in_text/2 found no answer~n", []), halt(1) ),
    maplist(codes_of, Shipped, ShippedCodes),
    (   ShippedCodes == MirrorUrls
    ->  true
    ;   format("[KO] urls_in_text/2 and r_urls_in_text/2 disagree~n  shipped ~q~n  mirror  ~q~n", [ShippedCodes, MirrorUrls]), halt(1) ),
    shortened_urls_in_text(Text, ShippedShort),
    (   user:r_shortened_urls_in_text(TextCodes, MirrorShort)
    ->  true
    ;   format("[KO] r_shortened_urls_in_text/2 found no answer~n", []), halt(1) ),
    maplist(codes_of, ShippedShort, ShippedShortCodes),
    (   ShippedShortCodes == MirrorShort
    ->  true
    ;   format("[KO] shortened_urls_in_text/2 disagrees with its mirror~n  shipped ~q~n  mirror  ~q~n", [ShippedShortCodes, MirrorShort]), halt(1) ),
    check_text(Texts).

check_html([]).
check_html([Html|Pages]) :-
    codes_of(Html, HtmlCodes),
    (   destination_url(Html, Shipped)
    ->  (   user:r_destination_url(HtmlCodes, MirrorCodes)
        ->  agree_chars("destination_url/2 and r_destination_url/2 disagree", Shipped, MirrorCodes)
        ;   format("[KO] the shipped module found a destination, the mirror found none~n", []), halt(1) )
    ;   (   user:r_destination_url(HtmlCodes, _)
        ->  format("[KO] the mirror found a destination, the shipped module found none~n", []), halt(1)
        ;   true )
    ),
    check_html(Pages).

check_rewrite([]).
check_rewrite([rewrite(Text, Pairs)|Rest]) :-
    rewrite_urls(Text, Pairs, Shipped),
    codes_of(Text, TextCodes),
    maplist(pair_codes, Pairs, PairCodes),
    (   user:r_rewrite_urls(TextCodes, PairCodes, MirrorCodes)
    ->  agree_chars("rewrite_urls/3 and r_rewrite_urls/3 disagree", Shipped, MirrorCodes)
    ;   format("[KO] r_rewrite_urls/3 found no answer~n", []), halt(1) ),
    check_rewrite(Rest).

pair_codes(Short-Expanded, ShortCodes-ExpandedCodes) :-
    codes_of(Short, ShortCodes),
    codes_of(Expanded, ExpandedCodes).

check_trim([]).
check_trim([Chars|Rest]) :-
    trim_trailing(Chars, Shipped),
    codes_of(Chars, Codes),
    (   user:r_trim_trailing(Codes, MirrorCodes)
    ->  agree_chars("trim_trailing/2 and r_trim_trailing/2 disagree", Shipped, MirrorCodes)
    ;   format("[KO] r_trim_trailing/2 found no answer~n", []), halt(1) ),
    check_trim(Rest).

%% The planner's mirror speaks the same terms as the planner — answers and
%% decisions, not characters — so only the addresses inside them are codes.
check_verdicts([]).
check_verdicts([Code|Codes]) :-
    refusal_status(Code, Shipped),
    (   user:k_refusal_status(Code, Mirror)
    ->  true
    ;   format("[KO] k_refusal_status/2 found no answer for ~w~n", [Code]), halt(1) ),
    % The mirror names a status with an atom where the shipped module uses a
    % char list, as it names a character with its code: the same allowed
    % translation, and the only one.
    atom_chars(Mirror, MirrorChars),
    (   Shipped == MirrorChars
    ->  true
    ;   format("[KO] status for ~w: shipped ~q, mirror ~q~n", [Code, Shipped, MirrorChars]), halt(1) ),
    check_verdicts(Codes).

corpus_rewrite([
    rewrite("Enquête 👉 https://l.mediapart.fr/MWq.", ["https://l.mediapart.fr/MWq"-"https://www.mediapart.fr/journal/france/enquete"]),
    rewrite("rien à faire ici", []),
    rewrite("a https://l.vert.eco/2V b https://l.marsactu.fr/ly8", ["https://l.vert.eco/2V"-"https://vert.eco/articles/le-sujet"])
]).

corpus_trim(["https://x.fr/2V.", "https://x.fr/2V", "https://x.fr/2V)»…", "abc"]).

corpus_code(200).
corpus_code(403).
corpus_code(404).
corpus_code(410).
corpus_code(429).
corpus_code(503).


%% The planner's mirror encodes two more things than the recogniser's, and
%% both are places a restatement can quietly stop meaning the same thing:
%%
%%   * a budget is a Peano numeral there (0, s(0), s(s(0))) where the shipped
%%     planner holds an integer and compares it with zcompare/3. "Never asks
%%     more than the budget allows" is proven about successor arithmetic, so
%%     the translation is checked here on every scenario;
%%   * an answer's status is an atom there and a char list in the ledger's
%%     vocabulary, the same translation the verdicts already use.
peano(0, 0).
peano(N, s(P)) :-
    N > 0,
    M is N - 1,
    peano(M, P).

known_codes([], []).
known_codes([Url-Answer|Rest], [Codes-MirrorAnswer|MirrorRest]) :-
    codes_of(Url, Codes),
    mirror_answer(Answer, MirrorAnswer),
    known_codes(Rest, MirrorRest).

mirror_answer(expanded(Url), expanded(Codes)) :-
    codes_of(Url, Codes).
mirror_answer(blocked, blocked).
mirror_answer(pending, pending).
mirror_answer(dead, dead).

mirror_decision(substitute(Url, Expanded), substitute(UrlCodes, ExpandedCodes)) :-
    codes_of(Url, UrlCodes),
    codes_of(Expanded, ExpandedCodes).
mirror_decision(leave(Url, Answer), leave(Codes, MirrorAnswer)) :-
    codes_of(Url, Codes),
    mirror_answer(Answer, MirrorAnswer).
mirror_decision(ask(Url), ask(Codes)) :-
    codes_of(Url, Codes).
mirror_decision(note(Url), note(Codes)) :-
    codes_of(Url, Codes).

check_decisions([]).
check_decisions([scenario(Urls, Known, Budget)|Rest]) :-
    expansion_decisions(Urls, Known, Budget, Shipped),
    maplist(codes_of, Urls, UrlCodes),
    known_codes(Known, KnownCodes),
    peano(Budget, Peano),
    (   user:k_decisions(UrlCodes, KnownCodes, Peano, Mirror)
    ->  true
    ;   format("[KO] k_decisions/4 found no answer for a budget of ~d~n", [Budget]), halt(1) ),
    maplist(mirror_decision, Shipped, ShippedMirrored),
    (   ShippedMirrored == Mirror
    ->  true
    ;   format("[KO] expansion_decisions/4 disagrees with its mirror~n  shipped ~q~n  mirror  ~q~n", [ShippedMirrored, Mirror]), halt(1) ),
    plan_of(Shipped, ShippedPlan),
    (   user:k_plan_of(Mirror, MirrorPlan)
    ->  true
    ;   format("[KO] k_plan_of/2 found no answer~n", []), halt(1) ),
    mirror_plan(ShippedPlan, ShippedPlanMirrored),
    (   ShippedPlanMirrored == MirrorPlan
    ->  true
    ;   format("[KO] plan_of/2 disagrees with its mirror~n  shipped ~q~n  mirror  ~q~n", [ShippedPlanMirrored, MirrorPlan]), halt(1) ),
    check_decisions(Rest).

mirror_plan(plan(substitutions(Pairs), ask(Ask), note(Note)),
            plan(substitutions(MirrorPairs), ask(MirrorAsk), note(MirrorNote))) :-
    maplist(pair_codes, Pairs, MirrorPairs),
    maplist(codes_of, Ask, MirrorAsk),
    maplist(codes_of, Note, MirrorNote).

check_answers([]).
check_answers([Outcome|Rest]) :-
    answer_of_outcome(Outcome, Shipped),
    mirror_outcome(Outcome, MirrorOutcome),
    (   user:k_answer_of_outcome(MirrorOutcome, Mirror)
    ->  true
    ;   format("[KO] k_answer_of_outcome/2 found no answer for ~q~n", [Outcome]), halt(1) ),
    mirror_answer(Shipped, ShippedMirrored),
    (   ShippedMirrored == Mirror
    ->  true
    ;   format("[KO] answer_of_outcome/2 disagrees for ~q: shipped ~q, mirror ~q~n", [Outcome, ShippedMirrored, Mirror]), halt(1) ),
    check_answers(Rest).

mirror_outcome(resolved(Url), resolved(Codes)) :-
    codes_of(Url, Codes).
mirror_outcome(refused(Code), refused(Code)).
mirror_outcome(unavailable(Reason), unavailable(Reason)).

corpus_scenario([
    scenario(["https://l.vert.eco/2V"], ["https://l.vert.eco/2V"-expanded("https://vert.eco/le-sujet")], 0),
    scenario(["https://l.vert.eco/2V", "https://l.marsactu.fr/ly8"], [], 1),
    scenario(["https://l.vert.eco/2V", "https://l.humanite.fr/AXQ", "https://bit.ly/4iLEDhE"],
             ["https://l.humanite.fr/AXQ"-blocked, "https://bit.ly/4iLEDhE"-dead], 2),
    scenario(["https://l.vert.eco/2V"], ["https://l.vert.eco/2V"-pending], 3),
    scenario([], [], 0)
]).

corpus_outcome([
    resolved("https://vert.eco/le-sujet"),
    refused(403),
    refused(404),
    refused(410),
    refused(503),
    unavailable(no_destination_in_page)
]).

run_test :-
    consult('proofs/kernel/recogniser'),
    consult('proofs/kernel/planner'),
    % The hand-written shapes, then every redirector host production uses and
    % the destinations the services handed back — generated from the collected
    % links rather than imagined.
    consult('tests/fixtures/link_corpus'),
    findall(Url, corpus_url(Url), Written),
    findall(Collected, user:corpus_link(Collected), Collected0),
    append(Written, Collected0, Urls),
    length(Urls, UrlCount),
    check_permalink(Urls),
    format("[ok] permalink/2 agrees with its mirror on ~d addresses~n", [UrlCount]),
    check_shortened(Urls),
    format("[ok] shortened_url_t/2 agrees with its mirror on ~d addresses~n", [UrlCount]),

    findall(Text, corpus_text(Text), Texts),
    length(Texts, TextCount),
    check_text(Texts),
    format("[ok] urls_in_text/2 and shortened_urls_in_text/2 agree on ~d posts~n", [TextCount]),

    findall(Html, corpus_html(Html), Pages),
    length(Pages, PageCount),
    check_html(Pages),
    format("[ok] destination_url/2 agrees with its mirror on ~d pages~n", [PageCount]),

    corpus_rewrite(Rewrites),
    length(Rewrites, RewriteCount),
    check_rewrite(Rewrites),
    format("[ok] rewrite_urls/3 agrees with its mirror on ~d substitutions~n", [RewriteCount]),

    corpus_trim(Trims),
    length(Trims, TrimCount),
    check_trim(Trims),
    format("[ok] trim_trailing/2 agrees with its mirror on ~d addresses~n", [TrimCount]),

    findall(Code, corpus_code(Code), Codes),
    length(Codes, CodeCount),
    check_verdicts(Codes),
    format("[ok] refusal_status/2 agrees with its mirror on ~d status codes~n", [CodeCount]),

    corpus_scenario(Scenarios),
    length(Scenarios, ScenarioCount),
    check_decisions(Scenarios),
    format("[ok] expansion_decisions/4 and plan_of/2 agree with their mirror on ~d scenarios, budget included~n", [ScenarioCount]),

    corpus_outcome(Outcomes),
    length(Outcomes, OutcomeCount),
    check_answers(Outcomes),
    format("[ok] answer_of_outcome/2 agrees with its mirror on ~d outcomes~n", [OutcomeCount]),

    format("[OK] proofs_bridge_test passed~n", []),
    halt(0).
