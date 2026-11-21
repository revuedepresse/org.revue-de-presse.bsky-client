/* LPTP mirror of src/link_shortener.pl, the recogniser that decides which of
   a post's links are a redirector's and what a fetched page says the reader
   should end up at.

   LPTP reads definite clauses plus negation of ground goals, and nothing
   else. The shipped module is written in the reified style this repository
   uses throughout -- if_/3, memberd_t/3, tfilter/3, zcompare/3, char_code/2,
   length/2, reverse/2, append/3 -- so this file restates the same relations
   in the fragment LPTP does read. It is plain Prolog with no directives
   because compile_gr/1 reads it and derives from it the clause-completion
   and induction axioms every proof beside it rests on.

   Nothing mechanical says this file agrees with src/link_shortener.pl, which
   is why that agreement is an obligation of its own (M1 in obligations.tsv)
   rather than something the proofs quietly assume. These are the encodings
   that obligation is about:

   1. A character is its code here, as it already is wherever the shipped
      module compares one: every classifier there begins with char_code/2 and
      then tests the code. So a post's text is mirrored as the list of its
      characters' codes, and every literal is spelled out the way
      src/link_shortener.pl already spells its character sets -- by code, in
      hex, with the string it stands for in a comment beside it. Writing the
      characters themselves is not open to us: LPTP's formula reader claims
      `?`, `&` and `=` as keywords of its own, and three of the sets below
      contain one of them, so a proof could not name those sets at all.

   2. if_(Condition, Then, Else) over a reified condition becomes a
      continuation predicate taking the condition's truth value, one clause
      per value -- which is the clausal dispatch the rest of this repository
      already writes by hand where no library call is involved. if_/3 over an
      equality becomes head unification plus one clause negating it; that
      negation is the only one in this file, and every call that reaches it
      has both arguments bound, which is the reading LPTP's `fails` gives it.
      A continuation takes the head and the tail of the list its caller was
      matching, rather than the list, so that every clause head here is
      first order and every induction step names the same variables the
      clause does.

   3. The condition a higher-order argument picks in take_while_not/4 and
      any_t/3 is named by a tag here, and r_cond_t/3 dispatches on it. LPTP
      has no call/N; a tag keeps one take_while_not and one any_t instead of
      a copy of each per condition, so the laws about them are proven once.

   4. length/2 followed by a range test becomes r_at_least/3 and r_at_most/3,
      which walk a unary numeral down the list. LPTP's fragment has no
      arithmetic, and counting against the list is what the range test means.
*/

%% r_memb(+X, +Xs, -Truth) -- mirror of memberd_t/3 on ground arguments.
r_memb(X, [X|_Ys], true).
r_memb(X, [Y|Ys], Truth) :- \+ X = Y, r_memb(X, Ys, Truth).
r_memb(_X, [], false).

/* r_cond_t(+Tag, +Char, -Truth) carries every character condition of the
   module. The tags name, in order: lower_t/2, upper_t/2, digit_t/2,
   token_char_t/2 (encoding 3 above), delimiter_t/2,
   trailing_punctuation_t/2, query_or_fragment_t/2, url_end_t/2,
   tag_close_t/2, quote_t/2, ampersand_t/2, and the two characters
   split_at_code/3 is called on -- `/` in url_host_path/3 and `?` in
   permalink/2. The last five stand for a code_t/3 call, which is a one
   character set and needs no separate shape. */
r_cond_t(lower, Char, Truth) :-
    r_memb(Char, [0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f,0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7a], Truth).                                      % a-z
r_cond_t(upper, Char, Truth) :-
    r_memb(Char, [0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4a,0x4b,0x4c,0x4d,0x4e,0x4f,0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5a], Truth).                                      % A-Z
r_cond_t(digit, Char, Truth) :-
    r_memb(Char, [0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39], Truth).   % 0-9
r_cond_t(delim, Char, Truth) :-
    r_memb(Char, [0x20,0x0a,0x0d,0x09,0x22,0x3c,0x3e,0xa0], Truth).    % space nl cr tab " < > nbsp
r_cond_t(punct, Char, Truth) :-
    r_memb(Char, [0x2e,0x2c,0x3b,0x3a,0x21,0x3f,0x29,0x22,0x27,0xbb,0x2026], Truth).  % . , ; : ! ? ) " ’ » …
r_cond_t(query, Char, Truth) :-
    r_memb(Char, [0x3f,0x23], Truth).                                      % ? #
r_cond_t(urlend, Char, Truth) :-
    r_memb(Char, [0x22,0x27,0x3b,0x20], Truth).                  % " ’ ; space
r_cond_t(tagclose, Char, Truth) :-
    r_memb(Char, [0x3e], Truth).                                            % >
r_cond_t(quote, Char, Truth) :-
    r_memb(Char, [0x22], Truth).                                            % "
r_cond_t(amp, Char, Truth) :-
    r_memb(Char, [0x26], Truth).                                            % &
r_cond_t(slash, Char, Truth) :-
    r_memb(Char, [0x2f], Truth).                                            % /
r_cond_t(question, Char, Truth) :-
    r_memb(Char, [0x3f], Truth).                                            % ?

/* token_char_t/2's if_/3 chain tests four disjoint sets of characters one
   after another, so it answers exactly `the character is in the union of the
   four', and that union is the table below -- a-z, A-Z, 0-9, hyphen and
   underscore. Mirroring the chain clause by clause instead would mean three
   continuation predicates, each needing its own functionality lemma, to say
   the same thing about the same inputs. */
r_cond_t(token, Char, Truth) :-
    r_memb(Char,
           [0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6a,0x6b,0x6c,0x6d,
           0x6e,0x6f,0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7a,
           0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4a,0x4b,0x4c,0x4d,
           0x4e,0x4f,0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5a,
           0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x2d,0x5f],
           Truth).

%% r_prefix_t(+Prefix, +Xs, -Truth) -- prefix_t/3, its if_(P = C, ...) turned
%  into head unification and one negated clause (encoding 2).
r_prefix_t([], _Xs, true).
r_prefix_t([_P|_Ps], [], false).
r_prefix_t([P|Ps], [P|Cs], Truth) :- r_prefix_t(Ps, Cs, Truth).
r_prefix_t([P|_Ps], [C|_Cs], false) :- \+ P = C.

%% r_strip_prefix(+Prefix, +Xs, -Rest)
r_strip_prefix([], Rest, Rest).
r_strip_prefix([P|Ps], [P|Cs], Rest) :- r_strip_prefix(Ps, Cs, Rest).

%% r_search(+Needle, +Haystack, -Result) -- found(Rest) or not_found.
r_search(_Needle, [], not_found).
r_search(Needle, [C|Cs], Result) :-
    r_prefix_t(Needle, [C|Cs], Truth),
    r_search_(Truth, Needle, C, Cs, Result).
r_search_(true, Needle, C, Cs, found(Rest)) :-
    r_strip_prefix(Needle, [C|Cs], Rest).
r_search_(false, Needle, _C, Cs, Result) :-
    r_search(Needle, Cs, Result).

%% r_contains_sub_t(+Needle, +Haystack, -Truth)
r_contains_sub_t(Needle, Haystack, Truth) :-
    r_search(Needle, Haystack, Result),
    r_found_truth(Result, Truth).

r_found_truth(not_found, false).
r_found_truth(found(_Rest), true).

%% r_split_at(+Tag, +Xs, -Result) -- split_at_code/3, the character it splits
%  on named by the tag its code_t/3 call tests for.
r_split_at(_Tag, [], not_split).
r_split_at(Tag, [C|Cs], Result) :-
    r_cond_t(Tag, C, Truth),
    r_split_at_(Truth, Tag, C, Cs, Result).
r_split_at_(true, _Tag, _C, Cs, split([], Cs)).
r_split_at_(false, Tag, C, Cs, Result) :-
    r_split_at(Tag, Cs, Inner),
    r_prepend_split(C, Inner, Result).

r_prepend_split(_C, not_split, not_split).
r_prepend_split(C, split(Before, After), split([C|Before], After)).

%% r_take_while_not(+Tag, +Xs, -Taken, -Rest)
r_take_while_not(_Tag, [], [], []).
r_take_while_not(Tag, [C|Cs], Taken, Rest) :-
    r_cond_t(Tag, C, Truth),
    r_take_while_not_(Truth, Tag, C, Cs, Taken, Rest).
r_take_while_not_(true, _Tag, C, Cs, [], [C|Cs]).
r_take_while_not_(false, Tag, C, Cs, [C|Taken], Rest) :-
    r_take_while_not(Tag, Cs, Taken, Rest).

/* append/3 and reverse/2 are library calls in the shipped module; LPTP needs
   their clauses. r_reverse/2 is the accumulator reverse the library uses. */
r_append([], Ys, Ys).
r_append([X|Xs], Ys, [X|Zs]) :- r_append(Xs, Ys, Zs).

%  reverse/2 of library(lists) walks the list with an accumulator; the
%  relation it computes is the one below, and this is the one the laws about
%  trim_trailing/2 are proven from, because an accumulator turns every
%  statement about the result into a statement about the accumulator too.
r_reverse([], []).
r_reverse([X|Xs], Ys) :- r_reverse(Xs, Rs), r_append(Rs, [X], Ys).

%% r_at_least(+N, +Xs, -Truth) -- N, a unary numeral, is at most the length of
%  Xs. With r_at_most/3 it replaces length/2 plus a range test (encoding 5).
r_at_least(z, _Xs, true).
r_at_least(s(_N), [], false).
r_at_least(s(N), [_X|Xs], Truth) :- r_at_least(N, Xs, Truth).

%% r_at_most(+N, +Xs, -Truth) -- the length of Xs is at most N.
r_at_most(_N, [], true).
r_at_most(z, [_X|_Xs], false).
r_at_most(s(N), [_X|Xs], Truth) :- r_at_most(N, Xs, Truth).

%% r_urls_in_text(+Chars, -Urls)
r_urls_in_text([], []).
r_urls_in_text([C|Cs], Urls) :-
    r_url_start_t([C|Cs], Truth),
    r_urls_in_text_(Truth, C, Cs, Urls).
r_urls_in_text_(true, C, Cs, [Url|More]) :-
    r_take_while_not(delim, [C|Cs], Raw, Rest),
    r_trim_trailing(Raw, Url),
    r_urls_in_text(Rest, More).
r_urls_in_text_(false, _C, Cs, Urls) :-
    r_urls_in_text(Cs, Urls).

%% r_url_start_t(+Chars, -Truth)
r_url_start_t(Chars, Truth) :-
    r_prefix_t([0x68,0x74,0x74,0x70,0x73,0x3a,0x2f,0x2f], Chars, Https),
    r_url_start_t_(Https, Chars, Truth).
r_url_start_t_(true, _Chars, true).
r_url_start_t_(false, Chars, Truth) :-
    r_prefix_t([0x68,0x74,0x74,0x70,0x3a,0x2f,0x2f], Chars, Truth).

%% r_trim_trailing(+Chars, -Trimmed)
r_trim_trailing(Chars, Trimmed) :-
    r_reverse(Chars, Reversed),
    r_drop_trailing(Reversed, Dropped),
    r_reverse(Dropped, Trimmed).

r_drop_trailing([], []).
r_drop_trailing([C|Cs], Out) :-
    r_cond_t(punct, C, Truth),
    r_drop_trailing_(Truth, C, Cs, Out).
r_drop_trailing_(true, _C, Cs, Out) :- r_drop_trailing(Cs, Out).
r_drop_trailing_(false, C, Cs, [C|Cs]).

%% r_shortened_urls_in_text(+Chars, -Urls)
r_shortened_urls_in_text(Chars, Shortened) :-
    r_urls_in_text(Chars, Urls),
    r_filter_shortened(Urls, WithDuplicates),
    r_unique(WithDuplicates, Shortened).

%% r_filter_shortened(+Urls, -Kept) -- tfilter(shortened_url_t, _, _).
r_filter_shortened([], []).
r_filter_shortened([U|Us], Kept) :-
    r_shortened_url_t(U, Truth),
    r_filter_shortened_(Truth, U, Us, Kept).
r_filter_shortened_(true, U, Us, [U|Kept]) :- r_filter_shortened(Us, Kept).
r_filter_shortened_(false, _U, Us, Kept) :- r_filter_shortened(Us, Kept).

r_unique([], []).
r_unique([X|Xs], [X|Rest]) :-
    r_without(X, Xs, Remaining),
    r_unique(Remaining, Rest).

%% r_without(+X, +Ys, -Kept) -- tfilter(dif_t(X), Ys, Kept): the elements of
%  Ys that are not X, in order.
r_without(_X, [], []).
r_without(X, [X|Ys], Kept) :- r_without(X, Ys, Kept).
r_without(X, [Y|Ys], [Y|Kept]) :- \+ X = Y, r_without(X, Ys, Kept).

%% r_rewrite_urls(+Chars, +Pairs, -Rewritten)
r_rewrite_urls([], _Pairs, []).
r_rewrite_urls([C|Cs], Pairs, Out) :-
    r_first_match([C|Cs], Pairs, Match),
    r_rewrite_step(Match, C, Cs, Pairs, Out).
r_rewrite_step(match(Expanded, Rest), _C, _Cs, Pairs, Out) :-
    r_append(Expanded, Tail, Out),
    r_rewrite_urls(Rest, Pairs, Tail).
r_rewrite_step(none, C, Cs, Pairs, [C|Tail]) :-
    r_rewrite_urls(Cs, Pairs, Tail).

r_first_match(_Chars, [], none).
r_first_match(Chars, [Short-Expanded|Pairs], Match) :-
    r_prefix_t(Short, Chars, Truth),
    r_first_match_(Truth, Short, Expanded, Chars, Pairs, Match).
r_first_match_(true, Short, Expanded, Chars, _Pairs, match(Expanded, Rest)) :-
    r_strip_prefix(Short, Chars, Rest).
r_first_match_(false, _Short, _Expanded, Chars, Pairs, Match) :-
    r_first_match(Chars, Pairs, Match).

%% r_shortened_url_t(+Url, -Truth)
r_shortened_url_t(Url, Truth) :-
    r_url_host_path(Url, Host, Path),
    r_generic_host_t(Host, Generic),
    r_shortened_url_t1(Generic, Host, Path, Truth).
r_shortened_url_t1(true, _Host, _Path, true).
r_shortened_url_t1(false, Host, Path, Truth) :-
    r_branded_redirector_t(Host, Branded),
    r_shortened_url_t2(Branded, Path, Truth).
r_shortened_url_t2(true, _Path, true).
r_shortened_url_t2(false, Path, Truth) :-
    r_opaque_short_path_t(Path, Truth).

%% r_generic_host_t(+Host, -Truth) -- memberd_t/3 against generic_shorteners/1.
r_generic_host_t(Host, Truth) :-
    r_memb(Host,
           [
            [0x62,0x69,0x74,0x2e,0x6c,0x79],               % bit.ly
            [0x62,0x75,0x66,0x66,0x2e,0x6c,0x79],          % buff.ly
            [0x64,0x6c,0x76,0x72,0x2e,0x69,0x74],          % dlvr.it
            [0x66,0x2e,0x6d,0x74,0x72,0x2e,0x63,0x6f,0x6f,0x6c], % f.mtr.cool
            [0x69,0x66,0x74,0x2e,0x74,0x74],               % ift.tt
            [0x6c,0x6e,0x6b,0x64,0x2e,0x69,0x6e],          % lnkd.in
            [0x6f,0x77,0x2e,0x6c,0x79],                    % ow.ly
            [0x70,0x75,0x6c,0x73,0x65,0x2e,0x6c,0x79],     % pulse.ly
            [0x73,0x68,0x6f,0x72,0x74,0x75,0x72,0x6c,0x2e,0x61,0x74], % shorturl.at
            [0x74,0x2e,0x63,0x6f],                         % t.co
            [0x74,0x69,0x6e,0x79,0x75,0x72,0x6c,0x2e,0x63,0x6f,0x6d], % tinyurl.com
            [0x74,0x72,0x69,0x62,0x2e,0x61,0x6c],          % trib.al
            [0x74,0x77,0x70,0x2e,0x61,0x69],               % twp.ai
            [0x75,0x72,0x6c,0x7a,0x2e,0x66,0x72],          % urlz.fr
            [0x79,0x6f,0x75,0x74,0x75,0x2e,0x62,0x65]      % youtu.be
           ],
           Truth).

r_branded_redirector_t(Host, Truth) :-
    r_prefix_t([0x6c,0x2e], Host, Ell),
    r_branded_redirector_t_(Ell, Host, Truth).
r_branded_redirector_t_(true, _Host, true).
r_branded_redirector_t_(false, Host, Truth) :- r_prefix_t([0x67,0x6f,0x2e], Host, Truth).

r_url_host_path(Url, Host, Path) :-
    r_url_after_scheme(Url, Rest),
    r_split_at(slash, Rest, Split),
    r_host_and_path(Split, Rest, Host, WithQuery),
    r_strip_query(WithQuery, Path).

r_host_and_path(not_split, Rest, Rest, [0x2f]).
r_host_and_path(split(Host, After), _Rest, Host, Path) :-
    r_append([0x2f], After, Path).

r_url_after_scheme(Url, Rest) :-
    r_prefix_t([0x68,0x74,0x74,0x70,0x73,0x3a,0x2f,0x2f], Url, Https),
    r_url_after_scheme_(Https, Url, Rest).
r_url_after_scheme_(true, Url, Rest) :- r_strip_prefix([0x68,0x74,0x74,0x70,0x73,0x3a,0x2f,0x2f], Url, Rest).
r_url_after_scheme_(false, Url, Rest) :- r_strip_prefix([0x68,0x74,0x74,0x70,0x3a,0x2f,0x2f], Url, Rest).

r_strip_query(Chars, Path) :- r_take_while_not(query, Chars, Path, _Rest).

r_opaque_short_path_t([], false).
r_opaque_short_path_t([C|Cs], Truth) :-
    r_cond_t(slash, C, Slash),
    r_opaque_short_path_t_(Slash, Cs, Truth).
r_opaque_short_path_t_(true, Cs, Truth) :- r_segment_truth(Cs, Truth).
r_opaque_short_path_t_(false, _Cs, false).

r_segment_truth(Segment, Truth) :-
    r_contains_sub_t([0x2f], Segment, Inner),
    r_segment_truth1(Inner, Segment, Truth).
r_segment_truth1(true, _Segment, false).
r_segment_truth1(false, Segment, Truth) :-
    r_len_in_t(Segment, Sized),
    r_segment_truth2(Sized, Segment, Truth).
r_segment_truth2(false, _Segment, false).
r_segment_truth2(true, Segment, Truth) :-
    r_all_token_chars_t(Segment, Tokens),
    r_segment_truth3(Tokens, Segment, Truth).
r_segment_truth3(false, _Segment, false).
r_segment_truth3(true, Segment, Truth) :- r_opaque_shape_t(Segment, Truth).

%% r_len_in_t(+Xs, -Truth) -- code_in_t(2, 14, Length, Truth) over the length
%  of Xs, as the two countings that range means (encoding 5).
r_len_in_t(Xs, Truth) :-
    r_at_least(s(s(z)), Xs, Two),
    r_len_in_t_(Two, Xs, Truth).
r_len_in_t_(false, _Xs, false).
r_len_in_t_(true, Xs, Truth) :-
    r_at_most(s(s(s(s(s(s(s(s(s(s(s(s(s(s(z)))))))))))))), Xs, Truth).

r_all_token_chars_t([], true).
r_all_token_chars_t([C|Cs], Truth) :-
    r_cond_t(token, C, Token),
    r_all_token_chars_t_(Token, Cs, Truth).
r_all_token_chars_t_(true, Cs, Truth) :- r_all_token_chars_t(Cs, Truth).
r_all_token_chars_t_(false, _Cs, false).

r_opaque_shape_t(Chars, Truth) :-
    r_any_t(digit, Chars, Digit),
    r_opaque_shape1(Digit, Chars, Truth).
r_opaque_shape1(true, Chars, Truth) :-
    r_any_t(lower, Chars, Lower),
    r_opaque_shape2(Lower, Chars, Truth).
r_opaque_shape1(false, Chars, Truth) :-
    r_any_t(lower, Chars, Lower),
    r_opaque_shape3(Lower, Chars, Truth).
r_opaque_shape2(true, _Chars, true).
r_opaque_shape2(false, Chars, Truth) :- r_any_t(upper, Chars, Truth).
r_opaque_shape3(true, Chars, Truth) :-
    r_any_t(upper, Chars, Upper),
    r_opaque_shape4(Upper, Chars, Truth).
r_opaque_shape3(false, Chars, Truth) :- r_short_enough_t(Chars, Truth).
r_opaque_shape4(true, _Chars, true).
r_opaque_shape4(false, Chars, Truth) :- r_short_enough_t(Chars, Truth).

%% r_short_enough_t(+Chars, -Truth) -- code_in_t(0, 4, Length, Truth): no
%  lower bound to check, since a length is never below zero.
r_short_enough_t(Chars, Truth) :- r_at_most(s(s(s(s(z)))), Chars, Truth).

r_any_t(_Tag, [], false).
r_any_t(Tag, [C|Cs], Truth) :-
    r_cond_t(Tag, C, Here),
    r_any_t_(Here, Tag, Cs, Truth).
r_any_t_(true, _Tag, _Cs, true).
r_any_t_(false, Tag, Cs, Truth) :- r_any_t(Tag, Cs, Truth).

%% r_page_destination(+Html, -Result) -- destination(Url) or none.
r_page_destination(Html, Result) :-
    r_tag_attribute([0x3c,0x6d,0x65,0x74,0x61], [0x72,0x65,0x66,0x72,0x65,0x73,0x68], [0x63,0x6f,0x6e,0x74,0x65,0x6e,0x74], Html, Refresh),
    r_refresh_destination(Refresh, Html, Result).

r_refresh_destination(attribute(Content), Html, Result) :-
    r_search([0x75,0x72,0x6c,0x3d], Content, AfterMarker),
    r_refresh_url(AfterMarker, Html, Result).
r_refresh_destination(none, Html, Result) :-
    r_claimed_destination(Html, Result).

r_refresh_url(found(After), _Html, destination(Url)) :-
    r_take_while_not(urlend, After, Url, _Rest).
r_refresh_url(not_found, Html, Result) :-
    r_claimed_destination(Html, Result).

r_claimed_destination(Html, Result) :-
    r_tag_attribute([0x3c,0x6c,0x69,0x6e,0x6b], [0x63,0x61,0x6e,0x6f,0x6e,0x69,0x63,0x61,0x6c], [0x68,0x72,0x65,0x66], Html, Canonical),
    r_canonical_destination(Canonical, Html, Result).

r_canonical_destination(attribute(Url), _Html, destination(Url)).
r_canonical_destination(none, Html, Result) :-
    r_tag_attribute([0x3c,0x6d,0x65,0x74,0x61], [0x6f,0x67,0x3a,0x75,0x72,0x6c], [0x63,0x6f,0x6e,0x74,0x65,0x6e,0x74], Html, OgUrl),
    r_og_destination(OgUrl, Result).

r_og_destination(attribute(Url), destination(Url)).
r_og_destination(none, none).

%% r_tag_attribute(+Open, +Mark, +Attribute, +Html, -Result)
r_tag_attribute(Open, Mark, Attribute, Html, Result) :-
    r_search(Open, Html, Found),
    r_tag_attribute1(Found, Open, Mark, Attribute, Result).
r_tag_attribute1(not_found, _Open, _Mark, _Attribute, none).
r_tag_attribute1(found(After), Open, Mark, Attribute, Result) :-
    r_take_while_not(tagclose, After, Tag, Rest),
    r_contains_sub_t(Mark, Tag, Marked),
    r_tag_attribute2(Marked, Tag, Open, Mark, Attribute, Rest, Result).
r_tag_attribute2(true, Tag, Open, Mark, Attribute, Rest, Result) :-
    r_attribute_value(Attribute, Tag, Inner),
    r_attribute_or_next(Inner, Open, Mark, Attribute, Rest, Result).
r_tag_attribute2(false, _Tag, Open, Mark, Attribute, Rest, Result) :-
    r_tag_attribute(Open, Mark, Attribute, Rest, Result).

r_attribute_or_next(attribute(Value), _Open, _Mark, _Attribute, _Rest,
                    attribute(Value)).
r_attribute_or_next(none, Open, Mark, Attribute, Rest, Result) :-
    r_tag_attribute(Open, Mark, Attribute, Rest, Result).

r_attribute_value(Name, Tag, Result) :-
    r_append(Name, [0x3d,0x22], Opening),
    r_search(Opening, Tag, Found),
    r_attribute_value_(Found, Result).
r_attribute_value_(not_found, none).
r_attribute_value_(found(After), attribute(Value)) :-
    r_take_while_not(quote, After, Value, _Rest).

r_destination_url(Html, Url) :- r_page_destination(Html, destination(Url)).

%% r_permalink(+Url, -Permalink)
r_permalink(Url, Permalink) :-
    r_split_at(question, Url, Split),
    r_permalink_(Split, Url, Permalink).
r_permalink_(not_split, Url, Url).
r_permalink_(split(Base, Query), _Url, Permalink) :-
    r_split_params(Query, Params),
    r_filter_functional(Params, Kept),
    r_rejoin(Base, Kept, Permalink).

r_split_params([], []).
r_split_params([C|Cs], [Param|Params]) :-
    r_take_while_not(amp, [C|Cs], Param, Rest),
    r_drop_ampersand(Rest, Remaining),
    r_split_params(Remaining, Params).

r_drop_ampersand([], []).
r_drop_ampersand([_C|Cs], Cs).

%% r_filter_functional(+Params, -Kept) -- tfilter(functional_param_t, _, _).
r_filter_functional([], []).
r_filter_functional([P|Ps], Kept) :-
    r_functional_param_t(P, Truth),
    r_filter_functional_(Truth, P, Ps, Kept).
r_filter_functional_(true, P, Ps, [P|Kept]) :- r_filter_functional(Ps, Kept).
r_filter_functional_(false, _P, Ps, Kept) :- r_filter_functional(Ps, Kept).

r_functional_param_t(Param, Truth) :-
    r_tracking_param_t(Param, Tracking),
    r_negate(Tracking, Truth).

r_negate(true, false).
r_negate(false, true).

r_tracking_param_t(Param, Truth) :-
    r_memb(Param, [[0x61,0x6d,0x70]], Exact),
    r_tracking_param_t_(Exact, Param, Truth).
r_tracking_param_t_(true, _Param, true).
r_tracking_param_t_(false, Param, Truth) :-
    r_tracking_prefixes(Prefixes),
    r_any_prefix_t(Prefixes, Param, Truth).

r_tracking_prefixes([
    [0x75,0x74,0x6d,0x5f],                         % utm_
    [0x61,0x74,0x5f],                              % at_
    [0x78,0x74,0x6f,0x72],                         % xtor
    [0x78,0x74,0x73],                              % xts
    [0x6d,0x63,0x5f],                              % mc_
    [0x70,0x6b,0x5f],                              % pk_
    [0x70,0x69,0x77,0x69,0x6b,0x5f],               % piwik_
    [0x66,0x62,0x63,0x6c,0x69,0x64],               % fbclid
    [0x67,0x63,0x6c,0x69,0x64],                    % gclid
    [0x67,0x62,0x72,0x61,0x69,0x64],               % gbraid
    [0x77,0x62,0x72,0x61,0x69,0x64],               % wbraid
    [0x69,0x67,0x73,0x68,0x69,0x64],               % igshid
    [0x6d,0x73,0x63,0x6c,0x6b,0x69,0x64],          % msclkid
    [0x74,0x77,0x63,0x6c,0x69,0x64],               % twclid
    [0x74,0x74,0x63,0x6c,0x69,0x64],               % ttclid
    [0x72,0x65,0x66,0x5f,0x73,0x72,0x63],          % ref_src
    [0x72,0x65,0x66,0x5f,0x75,0x72,0x6c],          % ref_url
    [0x63,0x6d,0x70,0x69,0x64],                    % cmpid
    [0x65,0x63,0x69,0x64],                         % ecid
    [0x66,0x65,0x61,0x74,0x75,0x72,0x65,0x3d,0x79,0x6f,0x75,0x74,0x75], % feature=youtu
    [0x73,0x70,0x6d],                              % spm
    [0x73,0x5f,0x6b,0x77,0x63,0x69,0x64],          % s_kwcid
    [0x73,0x72,0x5f,0x73,0x68,0x61,0x72,0x65]      % sr_share
]).

r_any_prefix_t([], _Param, false).
r_any_prefix_t([P|Ps], Param, Truth) :-
    r_prefix_t(P, Param, Here),
    r_any_prefix_t_(Here, Ps, Param, Truth).
r_any_prefix_t_(true, _Ps, _Param, true).
r_any_prefix_t_(false, Ps, Param, Truth) :- r_any_prefix_t(Ps, Param, Truth).

r_rejoin(Base, [], Base).
r_rejoin(Base, [P|Ps], Permalink) :-
    r_join_params([P|Ps], Query),
    r_append([0x3f], Query, WithMark),
    r_append(Base, WithMark, Permalink).

r_join_params([Param], Param).
r_join_params([Param, Next|Params], Joined) :-
    r_join_params([Next|Params], Rest),
    r_append([0x26], Rest, WithAmpersand),
    r_append(Param, WithAmpersand, Joined).

/* The rest of this file mirrors nothing in src/. These are the proof
   specifications the laws are stated against -- the shapes `a proper list',
   `no pair matches anywhere in this text', `only punctuation', `a contiguous
   piece of', `no duplicates' -- and they exist so that a law can be stated
   about every input rather than about one. */

%% r_list(+Xs) -- Xs is a proper list. Inducting over it is how a law about
%  every list of characters is proven, there being no type to induct over.
r_list([]).
r_list([_X|Xs]) :- r_list(Xs).

%% r_no_match(+Chars, +Pairs) -- no pair's short form occurs at any position
%  of Chars: first_match/3 answers none here and at every tail. This is what
%  `the text contains none of the pairs' means to rewrite_urls/3, which only
%  ever looks at a position and its tails.
r_no_match([], _Pairs).
r_no_match([C|Cs], Pairs) :-
    r_first_match([C|Cs], Pairs, none),
    r_no_match(Cs, Pairs).

%% r_all_punct(+Chars) -- every character is one trailing_punctuation_t/2
%  calls true.
r_all_punct([]).
r_all_punct([C|Cs]) :- r_cond_t(punct, C, true), r_all_punct(Cs).

%% r_in(+X, +Xs) -- X occurs in Xs. Obligation A6 asks for a SUBSET, and this
%  is the relation that says so. A sublist relation would be stronger, but
%  chaining two sublist facts needs an induction where chaining two subset
%  facts is two implications.
r_in(X, [X|_Ys]).
r_in(X, [_Y|Ys]) :- r_in(X, Ys).

%% r_notin(+X, +Xs) -- X occurs nowhere in Xs.
r_notin(_X, []).
r_notin(X, [Y|Ys]) :- \+ X = Y, r_notin(X, Ys).

%% r_nodup(+Xs) -- no element of Xs occurs twice in it.
r_nodup([]).
r_nodup([X|Xs]) :- r_notin(X, Xs), r_nodup(Xs).

%% r_dif_t(+X, +Y, -Truth) -- dif_t/3. The filter it is passed to,
%  tfilter(dif_t(X), Xs, Kept), is mirrored by r_without/3 above, where the
%  two verdicts have become the two clauses; this is the condition on its
%  own, so that the law about every reified condition of the module covers
%  this one as well.
r_dif_t(X, X, false).
r_dif_t(X, Y, true) :- \+ X = Y.

%% r_piece_of(+Xs, +Ys) -- Xs occurs in Ys as one contiguous run: Ys is
%  something, then Xs, then something. This is what obligation A5 means by a
%  contiguous subsequence, and the reason it is a law rather than a detail is
%  that urls_in_text/2 assembles its answers character by character: an
%  address put together out of pieces of a post, rather than cut out of it in
%  one piece, is a link nobody wrote.
r_piece_of(Xs, Ys) :-
    r_append(_Before, Tail, Ys),
    r_append(Xs, _After, Tail).
