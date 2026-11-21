# Machine-checked proofs of the link handling

This directory is a frame, not a result. It holds no proof yet.

LPTP (Stärk's Logic Program Theorem Prover, running on Scryer Prolog via
[contrib-lptp], branch `lptp_and_scryer-prolog`) machine-checks theorems about
pure Prolog. What it is here to check is the part of this collector that can
lose a link: the address rewriter in `src/link_shortener.pl`, the expansion
planner in `src/infrastructure/link/unfurl.pl`, and the ledger's verdicts in
`src/infrastructure/repository/repository_publication_link.pl`.

    make proofs-setup   # clone and build the checker into tmp/lptp (idempotent)
    make check-proofs   # check every proof file listed in check.pl; log in check.log

`check-proofs` reads its verdict out of `check.log`, not out of the exit
status: LPTP reports a rejected proof as a message and leaves Prolog's exit
code at 0, so a run in which every lemma failed looks, from the shell, exactly
like a clean one. It also counts the `o.k.` lines against the `check(` goals in
`check.pl`, because a proof file missing its `:- bye(<name>).` terminator is
never finalised and its lemmas are never checked -- and a file that was never
opened is as silent as one that passed.

## Why the denominator comes first

`obligations.tsv` is the point of this directory today. It lists one row per
law we intend to prove, written from the code paths that can lose links rather
than from the proofs that exist, so a law nobody has approached is a visible
open row instead of an absence. All fourteen rows are open. A list of proofs
could never have said that; neither can a test suite, because a test covers the
one input written in it and an obligation is a statement about every input.

Read the count with:

    make check-proof-obligations

## Why the last two groups are there

The planner rows (`P1`-`P3`) and the ledger row (`L1`) exist because an
unproven version of them cost 812 links. A first run met some
eight hundred shortened links it had never seen and asked about all of them
back to back; the redirector stopped answering, its `403` block pages were read
as a verdict on the links, and 812 live addresses were written off as dead. The
budget, the pacing, and the distinction between `blocked` and `dead` were the
fix. They are also exactly the kind of invariant a passing test cannot defend:
the budget is a counter threaded by hand through `expansion_pairs/4`, and
nothing but a proof says that thread is never dropped on some path through a
day's worth of links.

## Adding a proof

LPTP checks pure Prolog, not the shipped modules: a proof rests on a mirror of
the shipped clauses written in LPTP's fragment (reified conditions unfolded to
clausal dispatch, `if_/3` expanded, library calls restated). Put the mirror in
`kernel/`, compile it with `compile_gr($(proofs)/kernel/<name>)` in `check.pl`,
and add one `check($(proofs)/<file>)` goal per proof file. The mirror then owes
its own obligation: nothing mechanically establishes that it agrees with
`src/`, so that agreement belongs in `obligations.tsv` too, as a row, the day a
mirror exists.

Paths: the Makefile owns the only two absolute paths involved, `LPTP_ROOT_DIR`
and `PROOFS_DIR`. Every file reference goes through LPTP's `$(proofs)` alias,
because LPTP resolves a relative path against the directory of the file that
loaded it rather than the working directory.

[contrib-lptp]: https://github.com/thierrymarianne/contrib-lptp

## What is wanted and not proven

These belong here, in prose, rather than in `obligations.tsv`: a row in that
file means a law with a checked proof attached, so anything short of that has
to be written where no tool can read it as a guarantee.

- **`shortened_url_t/2` is total** — that every address gets a verdict rather
  than a failure. The bridge found a live instance the first time it ran over
  the collected corpus: given `chrome-error://chromewebdata/`, which the
  browser resolver had left among its results, the predicate fails instead of
  answering false, because `url_after_scheme/2` has no clause for an address
  that is neither http nor https. Nothing reaches it that way today —
  `shortened_urls_in_text/2` only ever passes it what `urls_in_text/2` found,
  and that is always http(s) — but `tfilter/3` over a failing condition fails
  the whole scan, and a post would then be stored with no expansion at all. A condition that fails instead of answering false makes the
  whole scan fail, and the post is then stored with no expansion at all. What
  is proven today is that there is at most one verdict and that it is true or
  false; that there is always one needs groundness reasoning these proofs do
  not set up.
- **The two mirrors are complete**, not merely in agreement on the corpus. The
  bridge compares them on the addresses the rankings actually carry; it does
  not prove the relations equal for every input.
