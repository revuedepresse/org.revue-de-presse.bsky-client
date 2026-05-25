# Realistic Fix Plan for the patched-VM SIGSEGV

Three tiers, ranked by effort/risk vs production impact. See
[`SIGSEGV-REPRODUCER.md`](./SIGSEGV-REPRODUCER.md) for the full
investigation report.

Recommendation: **ship Tier 1 + Tier 1.5 in the same change**.
Together they take roughly 1.5 hours and make the bug unreachable
in production. File Tier 2 upstream in parallel.

---

## Tier 1 — Ship the recovery patch (immediate)

**What.** Commit our `unify_constant` recovery into a scryer-prolog
fork, pin `deps/scryer-prolog` submodule to that SHA, redeploy.

**Patch** (`deps/scryer-prolog/src/machine/unify.rs`, replaces the
existing `b02e0c47` guard inside `fn unify_constant`):

```rust
let raw = ptr.get_ptr() as usize;

// The b02e0c47 alignment check is a no-op: ConsPtr::as_ptr left-
// shifts by NICHE_SHIFT = log2(align_of::<ArenaHeader>()), so the
// reconstructed pointer is 8-aligned by construction. The
// `< 0x10000` threshold also misses typical leak values (heap-
// index-shaped payloads ~10^6-10^7 shift to ~10^7-10^8).
//
// Real arena pointers on 64-bit systems live in ASLR regions
// (0x5555....., 0x7fff.....) -- always above 4 GiB. Anything below
// 4 GiB is either an inlined-atom payload or a tag-cleared heap
// reference -- in both cases re-interpreting as a Var pointing to
// the unshifted index is the least-bad behaviour: it converts a
// SIGSEGV into a graceful Prolog-level failure that catch/3 can
// handle.
#[cfg(target_pointer_width = "64")]
const PLAUSIBLE_MIN: usize = 0x1_0000_0000;
#[cfg(not(target_pointer_width = "64"))]
const PLAUSIBLE_MIN: usize = 0x10000;

if raw < PLAUSIBLE_MIN {
    let heap_index = raw >> ConsPtr::NICHE_SHIFT;
    let recovered_var = heap_loc_as_cell!(heap_index);
    self.pdl.push(value);
    self.pdl.push(recovered_var);
    return;
}

// Keep the original alignment check for any other corruption mode.
if raw % core::mem::align_of::<ArenaHeader>() != 0 {
    self.fail = true;
    return;
}
```

**Effort.** ~1 hour: commit into a scryer-prolog fork branch
(e.g., `revuedepresse/scryer-prolog:patched-with-recovery`), bump
the submodule SHA in this repo, `make docker-segv-reproducer-build`
(or `scryer-prolog-build`), `make app__bsky__feed__getAuthorFeed`
smoke test, deploy.

**Risk.** Low. Tested deterministically against the production
reproducer (3/3 SIGSEGVs eliminated). The recovery's "semantically
wrong for inlined atoms" path lands on the
`pg_query_silently_failed` handler that 48431e9 already
implements, so the codebase already knows how to deal with the
outcome.

**Production impact.** 77 %/batch crash rate → 77 %/batch
**skip** rate; batches complete, the worker stops dying, no
cron-restart-spam, supervisor stops fighting fires.

---

## Tier 1.5 — Project-level workaround (immediate, complementary)

**What.** The bisect showed the trigger requires `reif:if_/3`
somewhere in the SELECT-result path; removing it makes the bug
unreachable. Refactor `repository_status:exists_by_uri_t/3` to
use a direct comparison instead of `if_/3`.

**Before** (`src/infrastructure/repository/repository_status.pl`):

```prolog
exists_by_uri_t(Handle, URI, T) :-
    hash(handle(Handle)-uri(URI), Hash),
    count_matching_records(row(Hash), Count),
    if_(Count = 0, T = false, T = true).

count_matching_records(row(Hash), Result) :-
    pg_backend("wire"),
    count_matching_records_sql(SQL),
    value(SQL, [Hash], Result).      % wraps pg_query_or_throw + adapt_single_value (uses if_)
```

**After**:

```prolog
exists_by_uri_t(Handle, URI, T) :-
    hash(handle(Handle)-uri(URI), Hash),
    count_matching_records_raw(Hash, Count),
    ( Count =:= 0 -> T = false ; T = true ).

count_matching_records_raw(Hash, Count) :-
    count_matching_records_sql(SQL),
    pg_query(SQL, [Hash], Reply),
    interpret_count_reply(Reply, Count).

interpret_count_reply(data([[CountChars|_]|_]), Count) :-
    number_chars(Count, CountChars).
interpret_count_reply(error(Err), _) :-
    throw(pg_error(Err)).
```

Keep the original `count_matching_records/2`, `value/3`,
`adapt_single_value/2`, and the `psql` backend path -- the
refactor only affects `exists_by_uri_t/3`'s reduction.

**Effort.** ~30 min: edit the predicate, run
`make test-by-indexed-at-integration` (the existing test that
exercises `weaving_status` queries against `.env.local`), confirm
the production reproducer no longer crashes even on **unpatched**
scryer.

**Risk.** Low. Same SQL, same bind params, same connection. The
result interpretation path changes shape but the result data
contract is preserved.

**Effect alone.** Eliminates the trigger entirely. Works on the
existing production scryer binary without any VM rebuild.

---

## Tier 2 — Upstream scryer-prolog fix (long-term, file in parallel)

**What.** Fix the dispatch bug at the root in scryer-prolog itself.
`HeapCellValueTag::Cons = 0b000000` is a catch-all that conflates
arena pointers and inlined atoms. `unify_internal` at
`src/machine/unify.rs:462` treats every Cons-tagged cell as an
arena pointer and dereferences the payload, which segfaults for
inlined atoms.

**Two designs to propose:**

1. **`ConsPtrMaskTag`-based dispatch.** `ConsPtr` already has a
   1-bit sub-tag (`Cons=0` for arena, `Atom=1` for inline) at
   bit 63 of the cell. Dispatch on it in `unify_internal`:

   ```rust
   (HeapCellValueTag::Cons, ptr_1) => {
       let cp = ConsPtr::from_bytes(d1.into_bytes());
       match cp.get_tag() {
           ConsPtrMaskTag::Cons => Self::unify_constant(self, ptr_1, d2),
           ConsPtrMaskTag::Atom => Self::unify_inlined_atom(self, d1, d2),
       }
   }
   ```

   Requires an audit of every atom-constructing macro
   (`char_as_cell!`, `atom_as_cell!`) to confirm
   `ConsPtrMaskTag` is set correctly on construction. The bytes
   coming out of `AtomCell::new_char_inlined(...).into_bytes()`
   need bit 63 = `Atom` (1), not 0.

2. **Promote inlined atoms to a distinct `HeapCellValueTag`** --
   e.g., `HeapCellValueTag::InlineAtom = 0b000010` -- and update
   the atom-constructing macros to set it via
   `HeapCellValue::build_with` instead of the byte-preserving
   `from_bytes` cast. `unify_internal` then has a separate arm
   for inline atoms.

**Effort.** Unknown. Touches one of the hottest paths in the VM
(unify_internal runs on every term unification). Needs careful
benchmarking. Likely a multi-day effort for someone fluent in
scryer's atom/arena tag interplay.

**Filing.** Open an issue on `mthom/scryer-prolog` with:
- Link to `doc/SIGSEGV-REPRODUCER.md` in this branch
- `tests/pg/sigsegv_min_repro.pl` as an attached minimal example
- One of the preserved coredumps (231 MB) -- or a fresh one with
  the patched-with-recovery binary
- The two design proposals above

Production is **not blocked on this** once Tiers 1 + 1.5 ship.

---

## Why Tier 1 + Tier 1.5 together

| | Tier 1 alone | Tier 1.5 alone | Both |
|---|---|---|---|
| Works on existing binary in prod | ✗ | ✓ | ✓ |
| Works on a scryer rebuild without our patch | ✓ | ✓ | ✓ |
| Stops future code paths triggering the same bug | ✓ | partial | ✓ |
| Survives a rolling deploy mid-fix | partial | ✓ | ✓ |
| Production worker stops dying | ✓ | ✓ | ✓ |
| Posts process correctly (no skips) | ✗ -- still surfaces as skipped_post | ✓ | ✓ (Tier 1.5 path) |
| Effort | ~1 h | ~30 min | ~1.5 h |

Tier 1.5 alone is sufficient if we're sure the only triggering
predicate pair is `exists_by_uri_t/3 + repository_status:insert/3`.
Tier 1 is the safety net for any other code path (now or future)
that produces the same dispatch confusion. They're cheap; doing
both is the conservative choice.

---

## Step-by-step execution

Once you OK this plan:

### Tier 1 (1 hour)

1. Push `deps/scryer-prolog/src/machine/unify.rs` recovery diff
   to a `revuedepresse/scryer-prolog` fork on branch
   `unify-constant-recovery`.
2. Update `.gitmodules` if needed to point at the fork.
3. `cd deps/scryer-prolog && git checkout unify-constant-recovery`.
4. From repo root: `git add deps/scryer-prolog` (records the new
   submodule SHA).
5. `make docker-segv-reproducer-build` (or `scryer-prolog-build`).
6. Smoke test: `AUTHOR=lemonde.fr make app__bsky__feed__getAuthorFeed`
   on the production host -- should surface as `skipped_post`
   instead of SIGSEGV.
7. Commit submodule bump + push.

### Tier 1.5 (30 min)

1. Edit `src/infrastructure/repository/repository_status.pl` --
   replace `exists_by_uri_t/3` body per the snippet above.
2. Add `interpret_count_reply/2` and `count_matching_records_raw/2`
   helpers (private to the module).
3. Run existing tests:
   - `make test-by-indexed-at-integration` (read-only against prod
     DB)
   - The full reproducer suite -- expect all variants to now
     survive
4. Commit and push.

### Tier 2 (filed in parallel)

1. Open issue on `mthom/scryer-prolog` using the report content
   from `SIGSEGV-REPRODUCER.md` -- the "Crash signature", "Why
   the existing `b02e0c47` guard is ineffective", "Bisect history",
   "The actual scryer bug" sections in particular.
2. Attach `tests/pg/sigsegv_min_repro.pl`.
3. Link to the production observations and coredump paths.

---

## Status checkpoint

- [x] Investigation report (`doc/SIGSEGV-REPRODUCER.md`)
- [x] Minimal reproducer (`tests/pg/sigsegv_min_repro.pl`)
- [x] Recovery patch (Tier 1) -- upgraded to PLAUSIBLE_MIN=4 GiB +
      recovery-on-PDL (replaces earlier weak `< 0x10000` guard)
- [x] Tier 1 committed to fork branch `unify-constant-recovery`
      (2da7f36) + submodule bump recorded in main repo (99919da)
      **Needs push to GitHub** (`git push origin unify-constant-recovery`
      inside `deps/scryer-prolog` using personal credentials)
- [ ] Tier 1.5 (`exists_by_uri_t/3` refactor)
- [ ] Tier 2 upstream issue filed
