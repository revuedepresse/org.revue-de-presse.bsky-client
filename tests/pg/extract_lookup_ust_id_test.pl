:- module(extract_lookup_ust_id_test, [run_test/0]).

:- use_module(library(lists)).
:- use_module(library(si)).

:- use_module('../../src/infrastructure/repository/repository_status', [
    extract_lookup_ust_id/2
]).

/*
Regression tests for repository_status:extract_lookup_ust_id/2.

Production threw:

    could_not_iterate_over_all_author_feed_post(
        status_lookup_after_conflict_returned(
            data([[['1','4','0','6','6','9','1','3']]])))

`data([[['1','4',...]]])` is the wire-decode shape for one row with
one text cell (chars list). The pre-fix clause 2 reached the throw
even on this well-formed reply: it relied on
`dif(Reply, data([[_|_]|_]))`, but `dif/2` only POSTS a residual
constraint when the terms are unifiable. With the fresh `_`s in
the pattern, dif succeeded for any Reply, so once clause 1 was
skipped (or shadowed) clause 2 always threw.

The fix makes clause 1 self-validating (the cell must be a chars
list) and replaces the dif guard with negation-as-failure that
actually evaluates the shape predicate before throwing.
*/

run_test :-
    test_happy_path,
    test_null_cell_throws,
    test_empty_rows_throws,
    test_clause_two_alone_rejects_normal_shape,
    format("[OK] extract_lookup_ust_id_test passed~n", []).

% Wire-decode shape for 1 row, 1 text cell with value "14066913".
test_happy_path :-
    Reply = data([[['1','4','0','6','6','9','1','3']]]),
    extract_lookup_ust_id(Reply, UstIdChars),
    (   UstIdChars == ['1','4','0','6','6','9','1','3']
    ->  true
    ;   format("[FAIL] happy path: expected chars list, got ~q~n", [UstIdChars]),
        halt(1)
    ),
    (   chars_si(UstIdChars)
    ->  true
    ;   format("[FAIL] happy path: UstIdChars is not a chars list~n", []),
        halt(1)
    ).

% A row with a NULL ust_id must throw, not silently bind to `null`.
test_null_cell_throws :-
    Reply = data([[null]]),
    catch(
        ( extract_lookup_ust_id(Reply, Bound),
          format("[FAIL] null cell silently bound UstIdChars=~q~n", [Bound]),
          halt(1)
        ),
        status_lookup_after_conflict_returned(ThrownReply),
        (   ThrownReply == Reply
        ->  true
        ;   format("[FAIL] null-cell throw carried wrong reply ~q~n", [ThrownReply]),
            halt(1)
        )
    ).

% Empty result set: lookup returned no rows -> must throw.
test_empty_rows_throws :-
    Reply = data([]),
    catch(
        ( extract_lookup_ust_id(Reply, _),
          format("[FAIL] empty rows unexpectedly succeeded~n", []),
          halt(1)
        ),
        status_lookup_after_conflict_returned(ThrownReply),
        (   ThrownReply == Reply
        ->  true
        ;   format("[FAIL] empty-rows throw carried wrong reply ~q~n", [ThrownReply]),
            halt(1)
        )
    ).

% Pre-fix clause 2 (dif-based guard) is reproduced here in isolation
% to document the latent bug: dif posts a residual constraint and
% lets the throw fire even when the reply matches the expected
% shape. The fixed predicate avoids this by checking shape with
% \+ + chars_si rather than dif.
test_clause_two_alone_rejects_normal_shape :-
    Reply = data([[['1','4','0','6','6','9','1','3']]]),
    catch(
        ( clause_two_legacy(Reply, _),
          format("[FAIL] legacy clause-2 reproducer did not throw~n", []),
          halt(1)
        ),
        status_lookup_after_conflict_returned(ThrownReply),
        (   ThrownReply == Reply
        ->  true
        ;   format("[FAIL] legacy clause-2 throw carried wrong reply ~q~n", [ThrownReply]),
            halt(1)
        )
    ).

:- use_module(library(dif)).

clause_two_legacy(Reply, _) :-
    dif(Reply, data([[_|_]|_])),
    throw(status_lookup_after_conflict_returned(Reply)).

:- initialization((run_test, halt)).
