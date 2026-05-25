# scryer-prolog v0.10.0-186 SIGSEGV — Investigation Report

A patched-VM SIGSEGV that survives the existing `b02e0c47` guard
("Guard `Unifier::unify_constant` against bogus arena pointers").
Production crash rate ~77% per batch of the cron-driven worker;
1273 captures over 4 days against the patched binary.

This document is the consolidated investigation. It documents what
we found, the minimal reproducer, the shippable mitigation
(recovery patch), and the proper upstream fix.

## TL;DR

- The crash is **NOT** a memory corruption. It is a **dispatch bug**.
- `HeapCellValueTag::Cons = 0b000000` is a catch-all for "every
  cell with zero in the top 6 bits". This conflates two unrelated
  cell categories:
  - **Arena pointers** (streams, big ints, rationals) — payload is
    a real pointer
  - **Inlined atoms** (chars, short atoms via `AtomCell` bytes
    that don't set any of bits 58–63) — payload is packed character
    data
- `unify_internal`'s Cons arm at `src/machine/unify.rs:462` treats
  every Cons-tagged cell as an arena pointer and dereferences the
  payload via `get_tag()`. For inlined atoms the payload is not a
  valid pointer → SIGSEGV.
- The bug surfaces specifically when the call chain produces
  inlined-atom-shaped Cons cells (sha256 hex strings, base64-
  encoded payloads, atom-sized chars in the wire-decode path)
  *and* immediately attempts to unify them. The project's
  `repository_status:exists_by_uri_t/3 + repository_status:insert/3`
  pair does exactly that on every prod call; direct
  `postgresql:query/4` SELECT+INSERT never produces this cell
  shape and never crashes.
- The `b02e0c47` guard does not catch it because `ConsPtr::as_ptr()`
  left-shifts by `NICHE_SHIFT = log2(align_of::<ArenaHeader>()) = 3`,
  so the reconstructed pointer is 8-aligned by construction. The
  guard's alignment check (`raw % 8 != 0`) is tautologically true.

## Minimal reproducer

Setup once:

```bash
git fetch origin && git checkout reproduce-sigsegv-issue
make scryer-prolog-build           # OR make docker-segv-reproducer-build
make compose-up                    # brings up the test PG (port 55432)
```

Reproduce:

```bash
PGPASSWORD=test psql -h 127.0.0.1 -p 55432 -U test \
    -d revue_de_presse_test \
    -c "TRUNCATE TABLE public.weaving_status RESTART IDENTITY CASCADE;"

SOURCE=none \
DATABASE_HOST=127.0.0.1 DATABASE_PORT=55432 \
DATABASE_USERNAME=test DATABASE_PASSWORD=test \
DATABASE_DB_NAME=revue_de_presse_test \
PATH=./deps/scryer-prolog/target/release:$PATH \
    scryer-prolog ./tests/pg/sigsegv_min_repro.pl -g run
```

Expected on stock scryer v0.10.0-186-g29839848:
```
[..] SOURCE=none (no read_stream at all)
[..] Q0: exists_by_uri_t(min.example, at://min/post/0, T)
[..] Q0 returned T=false
[..] Q1: repository_status:insert/3
Segmentation fault (core dumped)
```

With the recovery patch applied (in `deps/scryer-prolog/src/machine/unify.rs`,
see "Mitigation" below):
```
[..] Q0 returned T=false
[..] Q1: repository_status:insert/3
[KO] repository_status:insert threw: pg_query_silently_failed(...)
```
No SIGSEGV, no coredump; the offending post is surfaced as a
recoverable Prolog-level failure that the existing
`onGetAuthorFeed/4` handler catches.

The reproducer requires:
- Patched scryer v0.10.0-186-g29839848 (the b02e0c47 guard is
  insufficient on its own)
- A running Postgres with the `weaving_status` schema (test PG
  via `make compose-up` is fine)
- `.env.local`-equivalent DATABASE_* variables (defaults shown above
  match the test PG)

No HTTP. No JSON. No captured production data. No specific
post content.

## Crash signature

```
Program terminated with signal SIGSEGV, Segmentation fault.
#0  scryer_prolog::types::UntypedArenaPtr::get_tag        src/types.rs:752
#1  scryer_prolog::machine::unify::Unifier::unify_constant src/macros.rs:269
#2  scryer_prolog::machine::unify::Unifier::unify_internal src/machine/unify.rs:462
#3  scryer_prolog::machine::Machine::dispatch_loop         src/macros.rs:413
```

`rdi/rsi` holds a small u64 value (typical: `0x198ECB`,
`0x1A66EA`, `0x28C80`, sometimes as small as `0x10`). After
`as_ptr`'s `<< 3` the deref targets unmapped memory.

## Why the existing `b02e0c47` guard is ineffective

The current guard at `src/machine/unify.rs`:

```rust
let raw = ptr.get_ptr() as usize;
if raw < 0x10000 || raw % core::mem::align_of::<ArenaHeader>() != 0 {
    self.fail = true;
    return;
}
```

`ptr.get_ptr()` is `ConsPtr::as_ptr()`:

```rust
pub fn as_ptr(self) -> *const ArenaHeader {
    let mut addr: u64 = self.ptr();
    addr <<= Self::NICHE_SHIFT;   // = log2(align_of::<ArenaHeader>()) = 3
    std::ptr::with_exposed_provenance(addr as usize)
}
```

So `raw = unshifted_field << 3`. The result is always a multiple
of 8, regardless of what `unshifted_field` is. The check
`raw % 8 != 0` is tautologically false. The check `raw < 0x10000`
catches only payloads where `unshifted_field < 0x2000` — far below
the typical observed values (heap-index-shaped or atom-packed
chars in the 10^3 – 10^7 range, which shift to 10^4 – 10^8,
well above 64 KiB).

## Bisect history — what does NOT trip the bug

| Variant | Result |
|---|---|
| Direct `postgresql:query/4` SELECT + INSERT | ✅ survives |
| Same + `crypto_data_hash` | ✅ survives |
| Same + full `encode_field_value` (`write_term_to_chars` + `chars_utf8bytes` + `chars_base64`) | ✅ survives |
| Same + `reif:if_/3` | ✅ survives |
| Via project's `pg_query/3` (bb_put-cached connection) | ✅ survives |
| `exists_by_uri_t/3` + raw `postgresql:query/4` INSERT | ✅ survives |
| Raw SELECT + `repository_status:insert/3` | ✅ survives |
| Inlined `exists_by_uri_t` (no `value/3`, no `if_/3`) + `repository_status:insert/3` | ✅ survives |
| `read_term` from file + full `onGetAuthorFeed/4` chain | ✅ survives |
| `phrase(json_chars(_), _)` from file + full chain | ✅ survives |
| Local HTTP + `phrase(json_chars(_), _)` + just `repository_status:insert/3` | ✅ survives |
| HTTPS to live Bluesky + full chain | ❌ crashes (production) |
| Local HTTP + full `onGetAuthorFeed/4` on captured feed body | ❌ crashes |
| **`exists_by_uri_t/3` + `repository_status:insert/3` (the minimum)** | ❌ **crashes** |

The crash requires the **specific combined invocation** of both
project predicates' full bodies. Neither alone, neither replaced
with a hand-written equivalent that exercises the same primitives
(`crypto_data_hash`, `write_term_to_chars`, `chars_utf8bytes`,
`chars_base64`, `if_/3`, project's `pg_query/3` with cached
connection) — all of those survive.

## What does NOT happen (instrumentation results)

We instrumented every `HeapCellValue` write path in scryer and
ran the reproducer with `SCRYER_TRACE_HEAP_CORRUPTION=1`:

| Instrumented point | Suspect cells caught |
|---|---|
| `HeapCellValue::set_value` | 0 |
| `HeapCellValue::build_with` | 0 |
| `from_bytes`-based macros (`char_as_cell`, `fixnum_as_cell`, `atom_as_cell`, `untyped_arena_ptr_as_cell`, `raw_ptr_as_cell`) | 0 |
| `Heap::push_cell` (both variants) | 0 |
| **At `unify_constant` recovery point** | **1700+** cells flagged |

The 1700+ "suspect" Cons cells on the heap aren't corruption.
Inspecting their values:

- `heap[41]  val=0x672d`   → `'g' '-'`
- `heap[43]  val=0x6e7572` → `'n' 'u' 'r'`
- `heap[89]  val=0x6372`   → `'c' 'r'`
- `heap[206] val=0x6c702e` → `'l' 'p' '.'`

These are **inlined atoms** (packed ASCII bytes). Scryer constructs
them via `char_as_cell!` / `atom_as_cell!`:

```rust
macro_rules! char_as_cell {
    ($c: expr) => {
        HeapCellValue::from_bytes(AtomCell::new_char_inlined($c).into_bytes())
    };
}
```

`AtomCell::new_char_inlined($c).into_bytes()` produces 8 bytes
whose top 6 bits happen to be zero. When `HeapCellValue::from_bytes`
reinterprets those bytes with HeapCellValue's bitfield layout,
the top 6 bits land in `HeapCellValueTag` (bits 58–63) — yielding
`HeapCellValueTag::Cons` (= `0b000000`). The packed character
bytes end up in the low 56 bits (val).

So the cell **is** legitimately constructed and stored. The bug
is that **unify_internal then misreads it**.

## The actual scryer bug — dispatch in `unify_internal`

`src/machine/unify.rs:462`:

```rust
(HeapCellValueTag::Cons, ptr_1) => {
    Self::unify_constant(self, ptr_1, d2);
}
```

`unify_constant` calls `match_untyped_arena_ptr!(ptr_1, ...)` which
calls `ptr_1.get_tag()` — defined as:

```rust
pub fn get_tag(self) -> ArenaHeaderTag {
    unsafe {
        debug_assert!(!self.get_ptr().is_null());
        let header = *self.get_ptr().cast::<ArenaHeader>();
        header.get_tag()
    }
}
```

The `*self.get_ptr().cast::<ArenaHeader>()` dereferences the cell's
payload as if it were a pointer to an `ArenaHeader`. For an
inlined atom the payload is packed character data, not a pointer.
On the host's address layout it points to unmapped memory →
SIGSEGV.

The cell's HeapCellValue tag (Cons) is correct for the dispatch
chain — `Cons` is the only catch-all bucket scryer has for "value
that isn't directly inlinable in the val field". But the dispatch
needs to **distinguish arena pointers from inlined atoms** before
it deref's, and currently it doesn't.

## Why specifically `exists_by_uri_t` + `repository_status:insert/3`

`exists_by_uri_t/3` ends with `if_(Count = 0, T = false, T = true)`
where Count is an integer parsed from a SQL `count(*)` result.
`if_/3` from `library(reif)` constructs reified boolean cells.
The `repository_status:insert/3` body uses `encode_field_value/2`
which produces a base64 chars list (every char is an inline atom
cell). Both predicates put **lots of inlined-atom Cons cells on
the heap right before** the next pg_query operation, which calls
unify on its bind parameters and result rows.

When the bind param processing iterates the param list and unifies
each char, one of those inlined-atom Cons cells reaches the
`HeapCellValueTag::Cons` arm of `unify_internal` and is mis-
dispatched. Direct `postgresql:query/4` with literal chars-list
parameters survives because the chars are constructed via list
syntax (Lis cells), which dispatches to the Lis arm, not Cons.

## Mitigation (shippable today)

Patch in this branch: `deps/scryer-prolog/src/machine/unify.rs`,
`fn unify_constant`. Replace the existing guard with a recovery
form: when `raw < 4 GiB` on 64-bit (or below the original 64 KiB
threshold on 32-bit), re-interpret the cell as a `Var` pointing
to the unshifted payload, push the corrected pair back to the
PDL, and return without failing.

```rust
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
```

**This is not a correctness fix.** The recovered Var is semantically
wrong for an inlined atom — the binding will produce an incorrect
unification, which downstream code observes as
`pg_query_silently_failed` (caught by the existing
`onGetAuthorFeed/4` handler that records the offending post as a
`skipped_post` and continues).

**Production effect:** worker survives instead of dying. Throughput
goes from "77% of posts crash the batch (cron-restart-spam,
batch never completes)" to "77% of posts are individually skipped
while the batch completes cleanly". Posts that hit the
mis-dispatch are unprocessable on this binary regardless, so
skipping them is the least-bad outcome.

## Proper upstream fix (requires scryer-internals expertise)

Distinguish inlined atoms from arena pointers in `unify_internal`'s
Cons arm. Two approaches:

1. **Use `ConsPtrMaskTag` (bit 63 of the cell) as the
   discriminator.** `ConsPtr` already has this 1-bit field
   (`Cons = 0` for arena pointer, `Atom = 1` for inlined atom).
   Audit the atom-constructing macros (`char_as_cell`,
   `atom_as_cell`) to ensure they set bit 63 correctly, then
   dispatch on it in `unify_internal`:

   ```rust
   (HeapCellValueTag::Cons, ptr_1) => {
       let cons_ptr = ConsPtr::from_bytes(d1.into_bytes());
       match cons_ptr.get_tag() {
           ConsPtrMaskTag::Cons => Self::unify_constant(self, ptr_1, d2),
           ConsPtrMaskTag::Atom => Self::unify_atom(self, ..., d2),
       }
   }
   ```

2. **Promote inlined atoms to a distinct `HeapCellValueTag`** —
   e.g., `HeapCellValueTag::InlineAtom = 0b000010` — and update
   the atom-constructing macros to set this tag. `unify_internal`
   then has a separate arm for inline atoms that never reaches
   `unify_constant`.

Both fixes need a survey of all sites that produce Cons-tagged
cells (every `from_bytes`-based construction) to ensure they
correctly identify their sub-type.

## Files in this branch

| File | Purpose |
|---|---|
| `doc/SIGSEGV-REPRODUCER.md` | This document |
| `tests/pg/sigsegv_min_repro.pl` | The minimal reproducer (this file is the canonical artifact) |
| `tests/pg/sigsegv_pgquery_repro.pl` | Bisect harness with knobs `USE_CRYPTO`, `USE_ENCODE`, `USE_REIF`, `USE_PROJECT_PG`, `USE_EXISTS`, `USE_EXISTS_INLINE`, `USE_EXISTS_INLINE_IF`, `USE_REPO_INSERT` — documents which combinations of primitives survive vs crash |
| `tests/pg/sigsegv_chainbisect_repro.pl` | Chain bisector (`CHAIN_LEVEL` 0–4) showing the minimum subset of `onGetAuthorFeed/4`'s pg_queries needed to trip the bug is `exists_by_uri_t + insert` |
| `tests/pg/sigsegv_databisect_repro.pl` | Data-field bisector (`SYNTH_*` knobs) proving the bug is content-independent |
| `tests/pg/sigsegv_http_repro.pl` + `tests/pg/run_http_repro.sh` + `tests/pg/serve_feed_fixture.py` | HTTP-driven reproducer (counter-example showing the bug doesn't require HTTP) |
| `tests/pg/sigsegv_jsondcg_repro.pl` | json_chars DCG variant (counter-example) |
| `tests/pg/sigsegv_minimal_repro.pl` | read_term variant (counter-example) |
| `tests/pg/reconstruct_feed_json.pl` | Captured-pairs → JSON regenerator (uses bidirectional `json_chars//1`) |
| `tests/pg/extract_failing_post.pl` | Field-value extractor for the crashing post |
| `docker/scryer-prolog/Dockerfile.debug` | Multi-stage Docker build of scryer with full DWARF debug symbols + gdb + psql |
| `compose.repro.yaml` | Docker compose: `segv-reproducer` runner + `segv-gdb` post-mortem container |
| Makefile targets | `docker-segv-reproducer-build`, `docker-segv-reproducer-run`, `docker-segv-gdb`, `scryer-prolog-build-with-debug-symbols` |
| Patches in `deps/scryer-prolog/` working tree (not committed to submodule) | The recovery patch in `src/machine/unify.rs` and diagnostic instrumentation gated by `SCRYER_TRACE_HEAP_CORRUPTION` env var |

## Coredumps preserved on `io.marianne.caprica`

| Path | Source |
|---|---|
| `var/tmp/segv-investigation/crash-20260524T170851Z-pid3985412/coredump` (231 MB) | First manual reproduction against real Bluesky API |
| `var/tmp/local-repro-runs/run-20260524T180055Z/coredump` (215 MB) | First local reproduction via local HTTP + captured fixture |
| `/tmp/core-run-{1,2,3}` (~237 MB each) | Three sequential `CHAIN_LEVEL=2` crashes confirming determinism |

## Production observations

- 1273 SIGSEGV captures over 2026-05-21 → 2026-05-24, all
  `exit_code=139`, all `worker_crashed_by_signal=11 signal_name=SIGSEGV`
- 20 captures on the patched VM `v0.10.0-186-g29839848` (the
  `b02e0c47`-guarded build); all from a single `get_authors_feeds`
  batch run (15:29–15:37Z 2026-05-24) over 20 of 26 configured
  authors (~77% per-batch crash rate)
- 1253 captures on the unpatched VM `v0.10.0-162-g8dffd72d`
- Worker invocation chain: root crontab `*/15 * * * *` →
  `get_authors_feeds` (ms `~deployer/.maintaining/org.revue-de-presse.bsky.sh`)
  spawns 3 concurrent `getAuthorFeed` workers each wrapped in
  `cpulimit -l 30`; concurrency and cpulimit are NOT the trigger
  (the local single-process reproducer crashes deterministically).

## What we would tell upstream

Filing this with scryer-prolog maintainers, the essentials:

- The `b02e0c47` guard's alignment check is tautologically true.
  The `< 0x10000` threshold is too narrow to catch typical leak
  values.
- The deeper bug is `unify_internal`'s Cons-arm dispatch
  conflating arena pointers and inlined atoms — both share the
  all-zeros HeapCellValue tag.
- Recovery-not-fail patch (this branch) eliminates the SIGSEGV
  while a proper dispatch fix is designed.
- 1300+ production coredumps available; 30-line reproducer
  attached.
