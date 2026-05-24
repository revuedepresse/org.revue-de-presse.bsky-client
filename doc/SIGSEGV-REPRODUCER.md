# scryer-prolog SIGSEGV reproducer (v0.10.0-186 / post-`b02e0c47`)

A patched-VM SIGSEGV that the `b02e0c47` guard ("Guard
`Unifier::unify_constant` against bogus arena pointers") does NOT
catch. The guard's alignment check is tautologically true because
it inspects the *post-niche-shift* pointer rather than the raw ptr
field — see [Why the existing guard is ineffective](#why-the-existing-guard-is-ineffective).

## Crash signature

Identical across all 1273 production captures and every reproducer
variant that trips the bug:

```
Program terminated with signal SIGSEGV, Segmentation fault.
#0  scryer_prolog::types::UntypedArenaPtr::get_tag         src/types.rs:752
#1  scryer_prolog::machine::unify::Unifier::unify_constant  src/macros.rs:269
#2  scryer_prolog::machine::unify::Unifier::unify_internal  src/machine/unify.rs:462
#3  scryer_prolog::machine::Machine::dispatch_loop          src/macros.rs:413
```

`rdi/rsi` carries a small-integer value (typical: `0x198ECB` =
1,674,955; `0x1A66EA` = 1,730,282) — the raw 64-bit payload of a
`HeapCellValueTag::Cons`-tagged heap cell that ought to hold a
real arena pointer.

## Minimum reproducer (after extensive bisecting)

The bug reproduces with just **two project predicate calls** against
an empty test PG. No HTTP, no JSON, no captures, no specific data.

```prolog
:- use_module('../../src/infrastructure/repository/repository_status', [
    exists_by_uri_t/3,
    insert/3
]).
:- use_module(library(assoc)).

run :-
    % DATABASE_* env vars must be set (e.g. against `make compose-up`'s test PG)
    exists_by_uri_t("anyhandle", "anyuri", _),
    empty_assoc(P),
    insert(
        row("d", "anyhandle", "t", "a", P, "anyuri", "2024-01-01T00:00:00.000Z"),
        _, _
    ).
    %% expected: SIGSEGV inside unify_constant before insert/3 returns
```

Setup:

```bash
make compose-up                                # bring up the test PG
PGPASSWORD=test psql -h 127.0.0.1 -p 55432 -U test \
  -d revue_de_presse_test \
  -c "TRUNCATE TABLE public.weaving_status RESTART IDENTITY CASCADE;"

DATABASE_HOST=127.0.0.1 DATABASE_PORT=55432 DATABASE_USERNAME=test \
DATABASE_PASSWORD=test DATABASE_DB_NAME=revue_de_presse_test \
PATH=./deps/scryer-prolog/target/release:$PATH \
  scryer-prolog ./tests/pg/sigsegv_pgquery_repro.pl -g run
  # with USE_EXISTS=1 USE_REPO_INSERT=1 to drive both project predicates
```

Reproduces 100% on first attempt against `make compose-up`'s
postgres:17-alpine container with the schema in
`docker/postgres/init/`.

## Bisect history — what does NOT trip the bug

The original symptom in production needed many ingredients (live
HTTPS to Bluesky + json_chars DCG parse of a 30-40 KB body +
multiple pg_query calls per post). The bisect peeled them off one
by one. **The following variants all SURVIVE** on the same patched
scryer:

| Variant | Tested via | Result |
|---|---|---|
| Direct `postgresql:query/4` SELECT + INSERT | `sigsegv_pgquery_repro.pl` (defaults) | ✅ survives |
| Same + `crypto_data_hash` between queries | `USE_CRYPTO=1` | ✅ survives |
| Same + `write_term_to_chars` + `chars_utf8bytes` + `chars_base64` (the full `encode_field_value` chain) | `USE_ENCODE=1` | ✅ survives |
| Same + `reif:if_/3` between queries | `USE_REIF=1` | ✅ survives |
| Via the project's `pg_query/3` (bb_put-cached connection) | `USE_PROJECT_PG=1` | ✅ survives |
| `repository_status:exists_by_uri_t/3` + raw `postgresql:query/4` INSERT | `USE_EXISTS=1` | ✅ survives |
| Raw SELECT + `repository_status:insert/3` | `USE_REPO_INSERT=1` | ✅ survives |
| Inlined `exists_by_uri_t` (append + crypto_data_hash + pg_query, no `value/3`, no `if_`) + `repository_status:insert/3` | `USE_EXISTS_INLINE=1 USE_REPO_INSERT=1` | ✅ survives |
| Inlined + trailing `if_/3` + `repository_status:insert/3` | `USE_EXISTS_INLINE_IF=1 USE_REPO_INSERT=1` | ✅ survives |
| Above with file-loaded JSON instead of http_open | `sigsegv_jsondcg_repro.pl` | ✅ survives |
| `read_term` from captured `last-feed-pairs.pl` + full `onGetAuthorFeed/4` chain | `sigsegv_minimal_repro.pl` | ✅ survives |

## What DOES trip the bug

| Variant | Result |
|---|---|
| **`repository_status:exists_by_uri_t/3` + `repository_status:insert/3`** | ❌ **CRASHES (deterministic, ~237 MB coredump)** |
| Production `onGetAuthorFeed/4` chain via real Bluesky HTTPS | ❌ crashes (1273 production captures since 2026-05-21) |
| Same via local http.server serving a regenerated feed-body.json | ❌ crashes on most captures (see commit history) |

So the trigger is the **specific combined invocation** of the two
project predicates. Neither one in isolation crashes; each replaced
with a hand-written near-equivalent (raw `query/4` calls plus the
underlying primitives one at a time — crypto, encoding, reif, project
pg wrapper) survives. The crash needs the *full bodies* of both
predicates to execute in sequence on the same connection.

The internal-only parts of `exists_by_uri_t` not yet bisected (the
real `value/3` + `adapt_single_value` + `pg_backend/1` + the trailing
`if_/3` reif comparison on the integer-coerced `Count`) are the
remaining suspect frontier. Further narrowing requires editing the
bodies of project predicates in place, which is beyond what
read-only test scaffolding can achieve.

## Why the existing `b02e0c47` guard is ineffective

`b02e0c47` added in `unify_constant`:

```rust
let raw = ptr.get_ptr() as usize;
if raw < 0x10000 || raw % core::mem::align_of::<ArenaHeader>() != 0 {
    self.fail = true;
    return;
}
```

The problem: `ConsPtr::as_ptr()` reconstructs the pointer by
**left-shifting** the stored 61-bit field by
`NICHE_SHIFT = log2(align_of::<ArenaHeader>()) = 3`:

```rust
pub fn as_ptr(self) -> *const ArenaHeader {
    let mut addr: u64 = self.ptr();
    addr <<= Self::NICHE_SHIFT;
    std::ptr::with_exposed_provenance(addr as usize)
}
```

`ArenaHeader` is `#[repr(align(8))]` (and 8 bytes). The shift is
`<< 3`, so **every reconstructed pointer is automatically 8-aligned
by construction** — the low 3 bits are zeros. The `raw % 8 != 0`
check can never trip.

The `raw < 0x10000` check also fails to catch typical bogus
payloads: an integer payload like `0x198ECB` shifted by 3 becomes
`0xCC7658` (~13 MB), well above 64 KiB.

### A guard that actually works

Validate the **unshifted** ptr field, not the reconstructed pointer:

```rust
// pseudo, exact API may differ
let unshifted: u64 = ptr.ptr();           // the 61-bit field, pre-shift
let raw_ptr   = ptr.get_ptr() as usize;   // post-shift
if unshifted < (0x10000 >> 3)
    || raw_ptr < 0x10000
{
    self.fail = true;
    return;
}
```

The minimal fix is checking the unshifted field. A real arena pointer
backed by a heap allocation has an unshifted field ≥ `mmap_min_addr
>> 3` (typically `0x2000` for the default 65 KiB mmap floor). Any
field below that is a small-integer leak.

## Reproducer files in this branch

- `tests/pg/sigsegv_pgquery_repro.pl` — the bisect harness (knobs:
  `USE_CRYPTO`, `USE_ENCODE`, `USE_REIF`, `USE_PROJECT_PG`,
  `USE_EXISTS`, `USE_EXISTS_INLINE`, `USE_EXISTS_INLINE_IF`,
  `USE_REPO_INSERT`)
- `tests/pg/sigsegv_min_repro.pl` — the absolute-minimum variant
  (just `exists_by_uri_t` + project `repository_status:insert/3`,
  no HTTP, no JSON)
- `tests/pg/sigsegv_chainbisect_repro.pl` — the chain bisector
  (`CHAIN_LEVEL` 0..4)
- `tests/pg/sigsegv_databisect_repro.pl` — the data-field bisector
  (`SYNTH_*` knobs proving the bug is data-independent)
- `tests/pg/sigsegv_http_repro.pl` + `tests/pg/run_http_repro.sh`
  + `tests/pg/serve_feed_fixture.py` — earlier HTTP-driven reproducer
  retained as a counter-example that demonstrates production fidelity
- `tests/pg/sigsegv_jsondcg_repro.pl` — file-loaded JSON variant
  (counter-example: survives)
- `tests/pg/sigsegv_minimal_repro.pl` — read_term variant
  (counter-example: survives)
- `tests/pg/reconstruct_feed_json.pl` — utility that regenerates
  the captured JSON via the bidirectional `json_chars//1` DCG
- `docker/scryer-prolog/Dockerfile.debug` + `compose.repro.yaml`
  + Makefile targets (`docker-segv-reproducer-build`,
  `docker-segv-reproducer-run`, `docker-segv-gdb`) — Docker-based
  scryer-prolog-with-debug-symbols build + gdb container

## Coredumps available

| Path on `io.marianne.caprica` | Source | Size |
|---|---|---|
| `var/tmp/segv-investigation/crash-20260524T170851Z-pid3985412/coredump` | First manual reproduction (lemonde.fr via real HTTPS) | 231 MB |
| `var/tmp/local-repro-runs/run-20260524T180055Z/coredump` | First local reproduction (pixelsfr capture via local HTTP) | 215 MB |
| `/tmp/core-run-{1,2,3}` | Three sequential `CHAIN_LEVEL=2` crashes proving determinism | ~237 MB each |
