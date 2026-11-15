:- module(link_shortener, [
    canonical_url/2,
    destination_url/2,
    meta_refresh_url/2,
    page_destination/2,
    permalink/2,
    rewrite_urls/3,
    shortened_url/1,
    shortened_url_t/2,
    shortened_urls_in_text/2,
    urls_in_text/2
]).

:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(reif)).

/**
Recognises the shortened links a post carries, so ingestion can store the
address a reader would end up at rather than a redirector's token.

Three shapes are treated as shortened:

  * a generic service (`bit.ly`, `buff.ly`, `tinyurl.com`, `t.co`, …);
  * a publisher's own redirector, which always sits on an `l.` or `go.`
    subdomain (`l.mediapart.fr`, `go.france24.com`, …);
  * a single opaque path segment on any host (`www.altereco.media/0BF`),
    since a publisher that shortens on its main domain looks like nothing
    else: two to fourteen characters, mixing digits with letters or
    cases, or short enough that no article slug would be mistaken for it.

It also reads, from a fetched page, where that page says the reader
should end up, and reduces an address to the article's own permalink.

Written in the reified style the rest of the codebase uses: conditions
are predicates whose last argument is a truth value, chosen with `if_/3`,
and searches hand back a tagged term (`found/1`, `value/1`, `match/2`,
`none`) that the caller dispatches on by pattern. No cuts, no
if-then-else, no negation as failure — every predicate here stays a
relation, which is also what makes them testable one case at a time.

Chars are compared by code throughout: several of the characters that
matter (`-`, `,`, `;`, `<`, `/`) are operator atoms, which Scryer will
not read in argument position even quoted.
*/

% -- reified character and list primitives ---------------------------------

%% code_in_t(+Low, +High, +Code, -Truth)
code_in_t(Low, High, Code, Truth) :-
    zcompare(BelowOrder, Low, Code),
    zcompare(AboveOrder, Code, High),
    range_truth(BelowOrder, AboveOrder, Truth).

range_truth(<, <, true).
range_truth(<, =, true).
range_truth(<, >, false).
range_truth(=, <, true).
range_truth(=, =, true).
range_truth(=, >, false).
range_truth(>, <, false).
range_truth(>, =, false).
range_truth(>, >, false).

lower_t(Char, Truth) :- char_code(Char, Code), code_in_t(97, 122, Code, Truth).
upper_t(Char, Truth) :- char_code(Char, Code), code_in_t(65, 90, Code, Truth).
digit_t(Char, Truth) :- char_code(Char, Code), code_in_t(48, 57, Code, Truth).

code_t(Expected, Char, Truth) :-
    char_code(Char, Code),
    =(Expected, Code, Truth).

token_char_t(Char, Truth) :-
    if_(lower_t(Char),
        Truth = true,
        if_(upper_t(Char),
            Truth = true,
            if_(digit_t(Char),
                Truth = true,
                if_(code_t(45, Char),           % -
                    Truth = true,
                    code_t(95, Char, Truth))))).% _

delimiter_t(Char, Truth) :-
    char_code(Char, Code),
    memberd_t(Code, [0x20, 0x0a, 0x0d, 0x09, 0x22, 0x3c, 0x3e, 0xa0], Truth).

trailing_punctuation_t(Char, Truth) :-
    char_code(Char, Code),
    memberd_t(Code,
              [0x2e, 0x2c, 0x3b, 0x3a, 0x21, 0x3f, 0x29, 0x22, 0x27, 0xbb, 0x2026],
              Truth).

%% prefix_t(+Prefix, +List, -Truth)
prefix_t([], _, true).
prefix_t([_|_], [], false).
prefix_t([P|Ps], [C|Cs], Truth) :-
    if_(P = C, prefix_t(Ps, Cs, Truth), Truth = false).

%% strip_prefix(+Prefix, +List, -Rest)
strip_prefix([], Rest, Rest).
strip_prefix([P|Ps], [P|Cs], Rest) :-
    strip_prefix(Ps, Cs, Rest).

%% search(+Needle, +Haystack, -Result)
%
% `found(Rest)` at the first occurrence, Rest being what follows it, or
% `not_found`.
search(_, [], not_found).
search(Needle, [C|Cs], Result) :-
    if_(prefix_t(Needle, [C|Cs]),
        ( strip_prefix(Needle, [C|Cs], Rest), Result = found(Rest) ),
        search(Needle, Cs, Result)).

contains_sub_t(Needle, Haystack, Truth) :-
    search(Needle, Haystack, Result),
    found_truth(Result, Truth).

found_truth(not_found, false).
found_truth(found(_), true).

%% split_at_code(+Code, +Chars, -Result)
%
% `split(Before, After)` at the first character with that code, else
% `not_split`.
split_at_code(_, [], not_split).
split_at_code(Code, [C|Cs], Result) :-
    if_(code_t(Code, C),
        Result = split([], Cs),
        ( split_at_code(Code, Cs, Inner), prepend_split(C, Inner, Result) )).

prepend_split(_, not_split, not_split).
prepend_split(C, split(Before, After), split([C|Before], After)).

%% take_while_not(:Condition, +Chars, -Taken, -Rest)
%
% Splits on the first character the reified Condition calls true; the
% character itself stays in Rest.
take_while_not(_, [], [], []).
take_while_not(Condition, [C|Cs], Taken, Rest) :-
    if_(call(Condition, C),
        ( Taken = [], Rest = [C|Cs] ),
        ( Taken = [C|Tail], take_while_not(Condition, Cs, Tail, Rest) )).

% -- addresses in a post ---------------------------------------------------

%% urls_in_text(+Chars, -Urls)
%
% Every http(s) address in the text, in order, with the punctuation a
% sentence leaves behind it stripped (`…voir https://x.fr/2V.` yields
% `https://x.fr/2V`).
urls_in_text([], []).
urls_in_text([C|Cs], Urls) :-
    if_(url_start_t([C|Cs]),
        ( take_while_not(delimiter_t, [C|Cs], Raw, Rest),
          trim_trailing(Raw, Url),
          Urls = [Url|More],
          urls_in_text(Rest, More) ),
        urls_in_text(Cs, Urls)).

url_start_t(Chars, Truth) :-
    if_(prefix_t("https://", Chars), Truth = true, prefix_t("http://", Chars, Truth)).

trim_trailing(Chars, Trimmed) :-
    reverse(Chars, Reversed),
    drop_trailing(Reversed, Dropped),
    reverse(Dropped, Trimmed).

drop_trailing([], []).
drop_trailing([C|Cs], Out) :-
    if_(trailing_punctuation_t(C),
        drop_trailing(Cs, Out),
        Out = [C|Cs]).

%% shortened_urls_in_text(+Chars, -Urls)
%
% The subset worth resolving, without duplicates: a post that repeats a
% link must not ask the service twice.
shortened_urls_in_text(Chars, Shortened) :-
    urls_in_text(Chars, Urls),
    tfilter(shortened_url_t, Urls, WithDuplicates),
    unique(WithDuplicates, Shortened).

unique([], []).
unique([X|Xs], [X|Rest]) :-
    tfilter(dif_t(X), Xs, Remaining),
    unique(Remaining, Rest).

dif_t(X, Y, Truth) :-
    if_(X = Y, Truth = false, Truth = true).

%% rewrite_urls(+Chars, +Pairs, -Rewritten)
%
% Replaces every occurrence of each `Short-Expanded` pair. Pairs whose
% short form does not occur leave the text untouched, so the caller can
% pass everything it knows.
rewrite_urls([], _, []).
rewrite_urls([C|Cs], Pairs, Out) :-
    first_match([C|Cs], Pairs, Match),
    rewrite_step(Match, [C|Cs], Pairs, Out).

rewrite_step(match(Expanded, Rest), _, Pairs, Out) :-
    append(Expanded, Tail, Out),
    rewrite_urls(Rest, Pairs, Tail).
rewrite_step(none, [C|Cs], Pairs, [C|Tail]) :-
    rewrite_urls(Cs, Pairs, Tail).

first_match(_, [], none).
first_match(Chars, [Short-Expanded|Pairs], Match) :-
    if_(prefix_t(Short, Chars),
        ( strip_prefix(Short, Chars, Rest), Match = match(Expanded, Rest) ),
        first_match(Chars, Pairs, Match)).

% -- what makes an address a redirector's ----------------------------------

%% shortened_url(+Url)
shortened_url(Url) :-
    shortened_url_t(Url, true).

%% shortened_url_t(+Url, -Truth)
shortened_url_t(Url, Truth) :-
    url_host_path(Url, Host, Path),
    generic_shorteners(Generic),
    if_(memberd_t(Host, Generic),
        Truth = true,
        if_(branded_redirector_t(Host),
            Truth = true,
            opaque_short_path_t(Path, Truth))).

generic_shorteners([
    "bit.ly", "buff.ly", "dlvr.it", "f.mtr.cool", "ift.tt", "lnkd.in",
    "ow.ly", "pulse.ly", "shorturl.at", "t.co", "tinyurl.com", "trib.al",
    "twp.ai", "urlz.fr", "youtu.be"
]).

branded_redirector_t(Host, Truth) :-
    if_(prefix_t("l.", Host), Truth = true, prefix_t("go.", Host, Truth)).

url_host_path(Url, Host, Path) :-
    url_after_scheme(Url, Rest),
    split_at_code(47, Rest, Split),             % /
    host_and_path(Split, Rest, Host, WithQuery),
    strip_query(WithQuery, Path).

host_and_path(not_split, Rest, Rest, "/").
host_and_path(split(Host, After), _, Host, [/|After]).

url_after_scheme(Url, Rest) :-
    if_(prefix_t("https://", Url),
        strip_prefix("https://", Url, Rest),
        strip_prefix("http://", Url, Rest)).

strip_query(Chars, Path) :-
    take_while_not(query_or_fragment_t, Chars, Path, _).

query_or_fragment_t(Char, Truth) :-
    char_code(Char, Code),
    memberd_t(Code, [0x3f, 0x23], Truth).       % ? #

opaque_short_path_t([], false).
opaque_short_path_t([C|Cs], Truth) :-
    if_(code_t(47, C),                          % /
        segment_truth(Cs, Truth),
        Truth = false).

segment_truth(Segment, Truth) :-
    if_(contains_sub_t("/", Segment),
        Truth = false,
        ( length(Segment, Length),
          if_(code_in_t(2, 14, Length),
              if_(all_token_chars_t(Segment),
                  opaque_shape_t(Segment, Truth),
                  Truth = false),
              Truth = false) )).

all_token_chars_t([], true).
all_token_chars_t([C|Cs], Truth) :-
    if_(token_char_t(C), all_token_chars_t(Cs, Truth), Truth = false).

% A token no article slug would be confused with: digits next to letters,
% both cases at once, or too short to say anything.
opaque_shape_t(Chars, Truth) :-
    if_(any_t(digit_t, Chars),
        if_(any_t(lower_t, Chars),
            Truth = true,
            any_t(upper_t, Chars, Truth)),
        if_(any_t(lower_t, Chars),
            if_(any_t(upper_t, Chars),
                Truth = true,
                short_enough_t(Chars, Truth)),
            short_enough_t(Chars, Truth))).

short_enough_t(Chars, Truth) :-
    length(Chars, Length),
    code_in_t(0, 4, Length, Truth).

any_t(_, [], false).
any_t(Condition, [C|Cs], Truth) :-
    if_(call(Condition, C), Truth = true, any_t(Condition, Cs, Truth)).

% -- what a fetched page says ----------------------------------------------

%% page_destination(+Html, -Result)
%
% `destination(Url)` or `none`. A redirector that bounces through
% JavaScript still serves a `<noscript>` fallback —
% `<meta http-equiv="refresh" content="0;url=…">` — which is the whole
% point of that fallback and the only thing a client without JavaScript
% can follow. An article page has no refresh and claims itself through
% `<link rel="canonical">`, or failing that `<meta property="og:url">`.
page_destination(Html, Result) :-
    tag_attribute("<meta", "refresh", "content", Html, Refresh),
    refresh_destination(Refresh, Html, Result).

refresh_destination(value(Content), Html, Result) :-
    search("url=", Content, AfterMarker),
    refresh_url(AfterMarker, Html, Result).
refresh_destination(none, Html, Result) :-
    claimed_destination(Html, Result).

refresh_url(found(After), _, destination(Url)) :-
    take_while_not(url_end_t, After, Url, _).
refresh_url(not_found, Html, Result) :-
    claimed_destination(Html, Result).

url_end_t(Char, Truth) :-
    char_code(Char, Code),
    memberd_t(Code, [0x22, 0x27, 0x3b, 0x20], Truth).   % " ' ; space

claimed_destination(Html, Result) :-
    tag_attribute("<link", "canonical", "href", Html, Canonical),
    canonical_destination(Canonical, Html, Result).

canonical_destination(value(Url), _, destination(Url)).
canonical_destination(none, Html, Result) :-
    tag_attribute("<meta", "og:url", "content", Html, OgUrl),
    og_destination(OgUrl, Result).

og_destination(value(Url), destination(Url)).
og_destination(none, none).

%% tag_attribute(+Open, +Mark, +Attribute, +Html, -Result)
%
% The attribute of the first tag opening with `Open` that mentions
% `Mark`; `value(V)` or `none`. Attribute order inside the tag does not
% matter: whole tags are read, then searched.
tag_attribute(Open, Mark, Attribute, Html, Result) :-
    search(Open, Html, Found),
    tag_attribute_(Found, Open, Mark, Attribute, Result).

tag_attribute_(not_found, _, _, _, none).
tag_attribute_(found(After), Open, Mark, Attribute, Result) :-
    take_while_not(tag_close_t, After, Tag, Rest),
    if_(contains_sub_t(Mark, Tag),
        ( attribute_value(Attribute, Tag, Inner),
          attribute_or_next(Inner, Open, Mark, Attribute, Rest, Result) ),
        tag_attribute(Open, Mark, Attribute, Rest, Result)).

attribute_or_next(value(Value), _, _, _, _, value(Value)).
attribute_or_next(none, Open, Mark, Attribute, Rest, Result) :-
    tag_attribute(Open, Mark, Attribute, Rest, Result).

tag_close_t(Char, Truth) :- code_t(0x3e, Char, Truth).      % >

attribute_value(Name, Tag, Result) :-
    append(Name, "=\"", Opening),
    search(Opening, Tag, Found),
    attribute_value_(Found, Result).

attribute_value_(not_found, none).
attribute_value_(found(After), value(Value)) :-
    take_while_not(quote_t, After, Value, _).

quote_t(Char, Truth) :- code_t(0x22, Char, Truth).          % "

%% canonical_url(+Html, -Url) and destination_url(+Html, -Url)
%
% Convenience views over page_destination/2 for callers that would rather
% fail than dispatch on a tag.
destination_url(Html, Url) :-
    page_destination(Html, destination(Url)).

canonical_url(Html, Url) :-
    claimed_destination(Html, destination(Url)).

meta_refresh_url(Html, Url) :-
    tag_attribute("<meta", "refresh", "content", Html, value(Content)),
    search("url=", Content, found(After)),
    take_while_not(url_end_t, After, Url, _).

% -- the article's own address ---------------------------------------------

%% permalink(+Url, -Permalink)
%
% Campaign parameters belong to the redirect that was followed, not to
% the piece: two posts linking one article through different campaigns
% would otherwise be stored as two addresses, and the publisher's
% analytics would travel into the archive and the feeds. Parameters
% without which an address stops working, such as YouTube's `v`, are
% kept.
permalink(Url, Permalink) :-
    split_at_code(0x3f, Url, Split),            % ?
    permalink_(Split, Url, Permalink).

permalink_(not_split, Url, Url).
permalink_(split(Base, Query), _, Permalink) :-
    split_params(Query, Params),
    tfilter(functional_param_t, Params, Kept),
    rejoin(Base, Kept, Permalink).

split_params([], []).
split_params([C|Cs], [Param|Params]) :-
    take_while_not(ampersand_t, [C|Cs], Param, Rest),
    drop_ampersand(Rest, Remaining),
    split_params(Remaining, Params).

drop_ampersand([], []).
drop_ampersand([_|Cs], Cs).

ampersand_t(Char, Truth) :- code_t(0x26, Char, Truth).      % &

functional_param_t(Param, Truth) :-
    if_(tracking_param_t(Param), Truth = false, Truth = true).

tracking_param_t(Param, Truth) :-
    tracking_exact(Exact),
    if_(memberd_t(Param, Exact),
        Truth = true,
        ( tracking_prefixes(Prefixes), any_prefix_t(Prefixes, Param, Truth) )).

any_prefix_t([], _, false).
any_prefix_t([P|Ps], Param, Truth) :-
    if_(prefix_t(P, Param), Truth = true, any_prefix_t(Ps, Param, Truth)).

% `amp` marks the accelerated copy of a page, which is the same article at
% a second address: dropped so both posts point at one permalink.
tracking_exact(["amp"]).

tracking_prefixes([
    "utm_", "at_", "xtor", "xts", "mc_", "pk_", "piwik_",
    "fbclid", "gclid", "gbraid", "wbraid", "igshid", "msclkid",
    "twclid", "ttclid", "ref_src", "ref_url", "cmpid", "ecid",
    "feature=youtu", "spm", "s_kwcid", "sr_share"
]).

rejoin(Base, [], Base).
rejoin(Base, [Param|Params], Permalink) :-
    join_params([Param|Params], Query),
    append("?", Query, WithMark),
    append(Base, WithMark, Permalink).

join_params([Param], Param).
join_params([Param, Next|Params], Joined) :-
    join_params([Next|Params], Rest),
    append("&", Rest, WithAmpersand),
    append(Param, WithAmpersand, Joined).
