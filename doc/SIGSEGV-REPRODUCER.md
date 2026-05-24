# scryer-prolog SIGSEGV reproducer (v0.10.0-186 / post-`b02e0c47`)

A patched-VM SIGSEGV that `b02e0c47` ("Guard `Unifier::unify_constant`
against bogus arena pointers") does NOT catch. The guard's
alignment check is tautologically true because it inspects the
post-niche-shift pointer rather than the raw ptr field — see
[Why the existing guard is ineffective](#why-the-existing-guard-is-ineffective)
below.

## Crash signature

Identical across production crashes (1273 captures under
`var/tmp/segv-investigation/`) and the local reproducer:

```
Program terminated with signal SIGSEGV, Segmentation fault.
#0  scryer_prolog::types::UntypedArenaPtr::get_tag        src/types.rs:752
#1  scryer_prolog::machine::unify::Unifier::unify_constant src/macros.rs:269
#2  scryer_prolog::machine::unify::Unifier::unify_internal src/machine/unify.rs:462
#3  scryer_prolog::machine::Machine::dispatch_loop         src/macros.rs:413
```

`rdi/rsi` holds a value like `0x198ECB` (1,674,955) or `0x1A66EA`
(1,730,282) — small integers in the range typical for the
`weaving_status.ust_id` column. They are the raw 64-bit payload of
a `HeapCellValueTag::Cons`-tagged heap cell that should hold an
arena pointer.

## Local reproducer

Self-contained on the same Linux x86_64 host that hits it in
production. No Bluesky API call, no HTTPS, no live remote network.

### Prerequisites

- Patched scryer-prolog v0.10.0-186-g29839848 built with debug
  symbols (`make docker-segv-reproducer-build` extracts the binary
  via Docker; see `docker/scryer-prolog/Dockerfile.debug`).
- A captured pair of `last-feed-pairs.pl` + `last-feed-body.txt`
  from a production crash. A working fixture is preserved at
  `var/tmp/local-repro-runs/run-20260524T180055Z/` on the prod host.
- A reachable Postgres instance with the production schema
  (`docker/postgres/init/03-status-schema.sql` defines
  `public.weaving_status` with the relevant `ust_hash` unique
  index).
- `.env.local` with `DATABASE_*` set.
- Python 3 (for the static feed server) on the test host.

### Recipe

```bash
# 1. Get the binary in place
make docker-segv-reproducer-build
docker create --name x org.revue-de-presse.bsky.segv-reproducer:debug
docker cp x:/usr/local/bin/scryer-prolog ./deps/scryer-prolog/target/release/scryer-prolog
docker rm x

# 2. Regenerate JSON body from a captured pair tree
INPUT=var/tmp/segv-investigation/crash-20260524T163515Z-pid3935536/last-feed-pairs.pl \
OUTPUT=var/tmp/segv-investigation/crash-20260524T163515Z-pid3935536/feed-body.json \
  scryer-prolog ./tests/pg/reconstruct_feed_json.pl -g run

# 3. Run the reproducer (starts local http.server, runs prolog, cleans up)
JSON_PATH=var/tmp/segv-investigation/crash-20260524T163515Z-pid3935536/feed-body.json \
POST_INDEX=0 \
PATH=./deps/scryer-prolog/target/release:$PATH \
  bash tests/pg/run_http_repro.sh
```

Expected: `Segmentation fault (core dumped)`, ~215 MB core file
matching the crash signature above.

### Reproducer rate

100% on first attempt against the production-host capture
`crash-20260524T163515Z-pid3935536` (author: `pixelsfr.bsky.social`),
POST_INDEX=0.

In production, 1273 captures over 4 days, all `exit_code=139`, ~77%
per-batch crash rate from the cron-driven `get_authors_feeds`
(launches 3 concurrent workers, each processing the 26-author
list).

## Why the existing guard is ineffective

`b02e0c47`'s guard in `unify_constant`:

```rust
let raw = ptr.get_ptr() as usize;
if raw < 0x10000 || raw % core::mem::align_of::<ArenaHeader>() != 0 {
    self.fail = true;
    return;
}
```

The problem: `ConsPtr::as_ptr()` reconstructs the pointer by
**left-shifting** the stored 61-bit field by `NICHE_SHIFT =
log2(align_of::<ArenaHeader>()) = 3`:

```rust
pub fn as_ptr(self) -> *const ArenaHeader {
    let mut addr: u64 = self.ptr();
    addr <<= Self::NICHE_SHIFT;
    std::ptr::with_exposed_provenance(addr as usize)
}
```

`ArenaHeader` is `#[repr(align(8))]` and 8 bytes, so the shift is
`<< 3`. **Every reconstructed pointer is automatically 8-aligned**
because the shift fills the low 3 bits with zeros. The `raw % 8 != 0`
check can never trip.

The `raw < 0x10000` check also fails to catch typical bogus
payloads: an integer like `0x198ECB` left-shifted by 3 becomes
`0xCC7658` (~13 MB), well above 64 KiB.

### A guard that actually works

Validate the unshifted ptr field, or impose a much higher threshold
on the reconstructed address:

```rust
let raw_field = ptr.into_bytes_u64() & ((1u64 << 61) - 1);  // unshifted
let raw_ptr = ptr.get_ptr() as usize;
if raw_field < (0x10000 >> 3)
    || raw_ptr < 0x10000
    || raw_ptr > MAX_REASONABLE_HEAP_ADDR
{
    self.fail = true;
    return;
}
```

The minimal fix is checking the unshifted field — a real arena
pointer's ptr-field is at least `mmap_min_addr >> 3` (typically
`0x2000` for the default 65 KiB mmap floor). Anything below that
is a small integer leak.

## Bisect history

| Variant | Result |
|---|---|
| `read_term` on captured `last-feed-pairs.pl` + `repository_status:insert/3` | ✅ no crash |
| `phrase(json_chars(pairs(_)), Chars)` on regenerated JSON + insert | ✅ no crash |
| `http_open` to local plain HTTP + json_chars + `repository_status:insert/3` (status insert only) | ✅ no crash |
| `http_open` to local plain HTTP + json_chars + `onGetAuthorFeed/4` (full chain: `exists_by_uri_t` + likes/reposts fetch + status insert + publication insert) | ❌ **CRASHES** on certain captures |

The full chain involves at least 3 `pg_query/3` calls per post.
Skipping the `exists_by_uri_t` check or the publication insert
avoids the crash. The bug is reachable only when the entire
sequence runs in one process.

Authors observed crashing in production batches (per
`fun.sh`'s capture manifests): all 18+ in the configured list,
including `lemonde.fr`, `pixelsfr.bsky.social`, `france24.com`,
`afrique.lemonde.fr`, `franceculture.fr`, `lefigaro.fr`,
`liberation.fr`, `lesechosfr.bsky.social`, etc.

## Investigation environment

- Production host: Debian on `sd-55237` (x86_64)
- Postgres reached via `database.local` (likely an SSH tunnel) on
  port 5434, user `io_marianne_caprica`, db `org_revue_de_presse`
- scryer submodule SHA: `29839848` (= patched `b02e0c47`)
- Worker repo HEAD when production crashes happen:
  `0b0b8a9` (`reproduce-sigsegv-issue` branch base is `0b0b8a9`)
- `cpulimit -l 30` wraps each worker in production (irrelevant to
  the reproducer — single-process repro tripping shows it is not
  the trigger)

## Files in this branch relevant to the reproducer

- `docker/scryer-prolog/Dockerfile.debug` — multi-stage build of
  scryer-prolog with full DWARF debug symbols
- `compose.repro.yaml` — Docker compose services for the
  reproducer container and a gdb-equipped post-mortem container
- `tests/pg/reconstruct_feed_json.pl` — regenerates the JSON body
  from a captured `last-feed-pairs.pl` via the bidirectional
  `json_chars//1` DCG
- `tests/pg/serve_feed_fixture.py` — minimal static-JSON HTTP
  server backing the local reproducer
- `tests/pg/sigsegv_http_repro.pl` — Prolog test that exercises
  the full `http_open → json_chars → onGetAuthorFeed/4` path
  against the local server
- `tests/pg/run_http_repro.sh` — orchestrator that starts the
  Python server, runs the test, cleans up
- `tests/pg/sigsegv_minimal_repro.pl` — counter-example: the
  read_term + insert path that does NOT crash on the same fixture
- `tests/pg/sigsegv_jsondcg_repro.pl` — counter-example: the
  json_chars-from-file + insert path that also does NOT crash
- `Makefile`: `docker-segv-reproducer-build`,
  `docker-segv-reproducer-run`, `docker-segv-gdb`
- `fun.sh`: `app__bsky__feed__getAuthorFeed` runs `ulimit -c
  unlimited` before scryer; `preserve_segv_captures` archives any
  `./core` alongside the Prolog-side captures
