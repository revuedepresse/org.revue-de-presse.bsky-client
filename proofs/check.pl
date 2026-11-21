/* The proof-check driver. Consulted AFTER LPTP itself, from the repo root:

       make check-proofs

   which runs, with the two paths the toolchain needs exported:

       scryer-prolog -g "consult('tmp/lptp/src/lptp.pl'),consult('proofs/check.pl')" -g halt

   PROOFS_DIR arrives from the Makefile and is bound to LPTP's $(proofs)
   alias, because LPTP resolves a relative path against the directory of the
   file that loaded it rather than the working directory: a proof file named
   relatively here would be looked for beside lptp.pl.

   Nothing here decides whether the proofs hold -- LPTP says that in its
   messages, and the Makefile reads them, since a rejected proof leaves the
   exit code at 0. */

:- use_module(library(os)).

main_proofs :-
    proofs_dir(Proofs),
    set(proofs, Proofs),
    check_all.

/* Two clauses rather than an if-then-else, as everywhere else in this
   repository: getenv/2 fails when the variable is absent, and the second
   clause is reached only then. check_all cannot fail, so a failure further
   on can never backtrack into it and blame a missing PROOFS_DIR. */
proofs_dir(Proofs) :-
    getenv("PROOFS_DIR", Chars),
    atom_chars(Proofs, Chars).
proofs_dir(_) :-
    write('check-proofs: PROOFS_DIR not set; run `make check-proofs` from the repo root'),
    nl,
    halt(1).

/* The proof files, one `check($(proofs)/...)` goal each, with any plain-Prolog
   mirror they rest on compiled first by `compile_gr($(proofs)/...)`.

   The Makefile counts the `check(` goals below against the o.k. lines LPTP
   prints, so adding a file here and nowhere else is enough to make the gate
   demand it. obligations.tsv stays the denominator either way: a proof file
   listed here discharges the rows it names there and no others. */
check_all :-
    compile_gr($(proofs)/kernel/recogniser),
    check($(proofs)/kernel/recogniser),
    compile_gr($(proofs)/kernel/planner),
    check($(proofs)/kernel/planner).

:- initialization(main_proofs).
