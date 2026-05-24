# PG backend switch — psql + shell ↔ postgresql-prolog wire

Date: 2026-05-24
Status: approved (brainstorming), implementation in progress

## Goal

Provide a runtime switch between two Postgres transports:

- **wire** — the current bind-param API on top of `deps/postgresql-prolog`
  (introduced in commit `69f3f97`, finished in `b2d726e`). Default.
- **psql** — the pre-`69f3f97` `psql(1)` + `shell/2` implementation,
  resurrected as an escape hatch in case the wire path destabilises
  (SIGSEGV under `pg_query/3`, scryer arena guard, etc.).

The switch is a single env var, `PG_BACKEND`, declared in `.env.local`.
Unset means `wire`, so existing deployments keep current behaviour with
no change. An empty value (`PG_BACKEND=`) is treated as operator error
and makes every dispatch clause fail — a Prolog failure at the call
site is easier to root-cause than a swallowed default.

## Non-goals

- We do **not** unify the two API surfaces. The wire API (`execute/2`,
  `value/3`, `query_result_from_file/4`, `matching_criteria/4`,
  `pg_query/3`) and the psql API (`query_result/2`,
  `query_result_from_file/3`, `matching_criteria/2`) coexist in
  `src/infrastructure/pg/client.pl`.
- We do **not** translate `$1, $2` placeholders at runtime inside the
  psql path. The psql clauses build concatenated SQL using
  `encode_field_value/2`, like the pre-`b2d726e` code did.
- We do **not** retroactively port every post-`b2d726e` feature into
  the psql clauses. Two features are explicitly mirrored
  (`clean_text/2` ingest cleaning, and the `dedup-by-URI` fix); the
  remainder are listed in §8 as known asymmetries.

## 1. Switch mechanism

### 1.1 Env var

`PG_BACKEND` in `.env.local`, `.env.local.dist`, `.env.test.dist`.
Values:

| Value | Backend | Notes |
| --- | --- | --- |
| (unset) | `wire` | default; no behaviour change vs today |
| `wire` | wire | postgresql-prolog over TCP, SCRAM-SHA-256 |
| `psql` | psql | `psql(1)` shell-out, `PGPASSWORD` env, pipe-delimited output |

### 1.2 Accessor — `src/configuration.pl`

Add `pg_backend/1` to the module exports and define it with **two
clauses, no cut**:

```prolog
pg_backend(Backend) :-
    getenv("PG_BACKEND", Backend).
pg_backend("wire") :-
    \+ getenv("PG_BACKEND", _).
```

Properties:

- `pg_backend("psql")` succeeds iff `PG_BACKEND=psql`.
- `pg_backend("wire")` succeeds iff `PG_BACKEND` is unset or
  equal to `"wire"`.
- An unknown value (e.g. `PG_BACKEND=foo`) makes every dispatch
  clause fail at its guard, producing a Prolog failure at the call
  site — easier to root-cause than a swallowed default. No throw
  wrapper.

No validation predicate, no `if_/3`. The two clauses are themselves
the discriminator; backtracking lands on the matching one.

## 2. `src/infrastructure/pg/client.pl` — both API surfaces

### 2.1 Exports

```prolog
:- module(client, [
    %% wire-era (current, kept verbatim)
    execute/2,
    pg_query_or_throw/3,
    query_result_from_file/4,
    matching_criteria/4,
    record_pg_query_failure/3,
    value/3,

    %% psql-era (revived from 77bcc36:src/infrastructure/pg/client.pl)
    matching_criteria/2,
    query_result/2,
    query_result_from_file/3,

    %% shared
    encode_field_value/2,
    hash/2,
    read_rows/2
]).
```

### 2.2 Revival source

The psql-era predicates are lifted verbatim from
`git show 77bcc36:src/infrastructure/pg/client.pl` — the last commit
touching `client.pl` before `69f3f97` replaced it with the wire path.

### 2.3 One fix to the lifted code: separator

The historical psql invocation used `--csv`, which emits comma-separated
rows. The current `repository_dcgs:rows//1` DCG parses **pipes**. The
two never agreed on multi-column rows; this only worked because the
multi-column path (`matching_criteria/2`) was rarely exercised in
practice.

Fix: replace `--csv` in the lifted command line with
`-A -F '|' -P null=` (unaligned output, pipe field separator, empty
representation of `NULL`). Output then matches the format the wire
path's `write_field/2` already writes, and `read_rows/2` (shared)
parses both transparently.

### 2.4 Imports

Both `library(os)` (psql path) and the local `connection` module
(wire path) are imported. Importing `connection.pl` is safe in
`PG_BACKEND=psql` runs — the TCP socket is opened lazily inside
`pg_query/3`, never on module load.

### 2.5 Failure-capture file

`record_pg_query_failure/3` stays wire-only. It logs `SQL + Params`
from the bind-param API, which doesn't exist in psql mode. The psql
path's existing failure surface (`shell/2` non-zero exit →
`unexpected_command_exit_code` throw) is preserved as-is.

## 3. Per-repository dispatch — the rule

A predicate gets **two clauses**, guarded by `pg_backend/1` and **no
cut**, iff its body directly calls one of the four wire-era client
predicates:

- `client:execute/2`
- `client:value/3`
- `client:query_result_from_file/4`
- `client:matching_criteria/4`

or directly calls `connection:pg_query/3`.

Every such predicate gains a paired clause guarded by
`pg_backend("psql")` that routes through the psql-era API
(`query_result/2`, `query_result_from_file/3`, `matching_criteria/2`)
with concatenated SQL built via `encode_field_value/2` and
`append/2`.

Predicates **above** that seam — pure composition like
`exists_by_uri_t/3`, validators, hashing — stay single-bodied. They
work on both backends because the seam underneath has been switched.

### 3.1 Inventory — predicates that need dual clauses

(file:line — outermost predicate name)

**`repository_list.pl`**

- `count/1` (line 72) — `value/3`
- `query/3` (line 128) — `query_result_from_file/4`
- `query_max_id/1` (line 137 + 148) — `value/3` ×2
- `by_at_uri/2` (line 222) — `query_result_from_file/4`
- `do_select/3` (line 253) — `value/3`
- `do_insert/3` (line 281) — `execute/2`
- `select_existing_by_at_uri/2` (line 286) — `value/3`

**`repository_list_item.pl`**

- `count/1` (line 77) — `value/3`
- `query/3` (line 135) — `query_result_from_file/4`
- `query_max_id/1` (line 143) — `value/3`
- (line 175, 207) — `query_result_from_file/4` ×2
- (line 292) — `execute/2`
- (line 296) — `value/3`
- (line 336) — `value/3`
- (line 395) — `execute/2`
- (line 403) — `value/3`
- (line 447) — `value/3`

**`repository_popularity.pl`**

- `count/1` (line 71) — `value/3`
- `query/3` (line 125) — `query_result_from_file/4`
- `query_max_id/1` (line 133) — `value/3`
- (line 164) — `matching_criteria/4`
- (line 216) — `execute/2`
- (line 221) — `matching_criteria/4`
- (line 294) — `pg_query/3`
- (line 318) — `value/3`

**`repository_publication.pl`**

- `count/1` (line 70) — `value/3`
- `query/3` (line 121) — `query_result_from_file/4`
- `query_max_id/1` (line 129) — `value/3`
- (line 162) — `matching_criteria/4`
- (line 220) — `pg_query/3`
- (line 246) — `value/3`

**`repository_publisher.pl`**

- `count/1` (line 65) — `value/3`
- `query/3` (line 119) — `query_result_from_file/4`
- `query_max_id/1` (line 127) — `value/3`
- (line 158) — `matching_criteria/4`
- (line 205) — `execute/2`
- (line 209) — `value/3`
- (line 247) — `value/3`

**`repository_status.pl`**

- `count/1` (line 75) — `value/3`
- `query/3` (line 128) — `query_result_from_file/4`
- `query_max_id/1` (line 136) — `value/3`
- (line 168) — `matching_criteria/4`
- (line 230) — `pg_query/3` (insert RETURNING)
- (line 252) — `pg_query/3` (extract_lookup_ust_id)
- (line 293) — `matching_criteria/4`
- `count_matching_records/2` (line 336) — `value/3` — **dedup-by-URI seam**
- `by_indexed_at/2` (line 378) — `matching_criteria/4`

### 3.2 Psql clause shape

```prolog
public_predicate(Args…) :-
    pg_backend("wire"),
    %% existing body unchanged: $N SQL, client:execute/value/etc.

public_predicate(Args…) :-
    pg_backend("psql"),
    %% concatenated SQL via encode_field_value/append,
    %% routed through client:query_result/2 (or _from_file/3, or matching_criteria/2).
```

No `!`. `pg_backend("wire")` and `pg_backend("psql")` are mutually
exclusive at runtime, so the failing clause backs out at its guard
without entering the body.

### 3.3 `pg_query/3` call sites

The four direct callers of `connection:pg_query/3` translate into
psql clauses that emit the equivalent `INSERT … RETURNING <col>` (or
plain `SELECT`) via `query_result/2` against concatenated SQL:

- `repository_status.pl:230` — `INSERT … RETURNING ust_id`
- `repository_status.pl:252` — `SELECT ust_id FROM weaving_status WHERE ust_hash = $1`
  (lookup after conflict)
- `repository_publication.pl:220` — publication `INSERT … RETURNING`
- `repository_popularity.pl:294` — popularity `INSERT … RETURNING`

Values are encoded with `encode_field_value/2` as in the resurrected
client.

## 4. Features ported to both paths

### 4.1 `clean_text/2` on ingest

Applied at three sites in the wire-era bodies:

- `repository_status.pl:225` — `PreQuotingText` (post body)
- `repository_publication.pl:216` — `PreQuotingText` (post body, publication table)
- `repository_list_item.pl:378–379` — `DisplayName` + `Description`

The psql clauses for the same three predicates call
`clean_text(Raw, Cleaned)` at the same position, immediately before
`encode_field_value(Cleaned, _)`. The `:- use_module('../../clean_text',
[clean_text/2]).` import already exists in those three files; no
additional imports.

### 4.2 Dedup-by-URI fix (commit `b6f46bf`)

`exists_by_uri_t/3` is pure composition over `hash/2`,
`count_matching_records/2`, and `if_/3`. The wire/psql seam sits in
`count_matching_records/2`, which is in the inventory in §3.1. Once
`count_matching_records/2` has dual clauses, `exists_by_uri_t/3` works
on both backends without itself branching — same call sites
(`event_getAuthorFeed:onGetAuthorFeed/2`), same observable behaviour.

## 5. Tests

CI runs against the existing Postgres service container
(`postgres:17-alpine` on port `55432`). No new container needed.

### 5.1 Re-run existing pg tests under `PG_BACKEND=psql`

Add one CI step (and one Makefile target) per existing pg test,
exporting `PG_BACKEND=psql`:

- `test-repository-inserts-psql` → `tests/pg/repository_inserts_test.pl`
- `test-already-indexed-by-uri-psql` → `tests/pg/already_indexed_by_uri_test.pl`
- `test-repository-list-psql` → `tests/pg/repository_list_test.pl`
- `test-repository-publisher-psql` → `tests/pg/repository_publisher_test.pl`

These tests verify rows via `pg_query_simple/2` (wire) — that's fine
even with `PG_BACKEND=psql`. The system-under-test writes through psql;
teardown/verification reads via wire. Same DB, two transports.

### 5.2 New: `tests/pg/clean_text_psql_test.pl`

Smallest test that fails if the psql clause forgot to call
`clean_text/2`:

1. CI step exports `PG_BACKEND=psql` (env-level, not via `setenv`
   inside the test).
2. `pg_query_simple/2` deletes any pre-existing row keyed on a probe
   URI.
3. `repository_status:insert/3` is called with a `PreQuotingText`
   carrying a known dirt pattern (same input as `tests/clean_text_test.pl`).
4. `pg_query_simple/2` selects the row back and asserts `ust_text`
   equals `clean_text/2`'s expected output — not the raw input.

Makefile target `test-clean-text-psql`. New CI step.

### 5.3 Backend-default smoke

A two-line addition (or tiny new file `tests/pg/backend_default_test.pl`)
asserts that `pg_backend("wire")` succeeds when `PG_BACKEND` is unset.
One CI step with the variable explicitly cleared.

## 6. Operational details

- **psql binary**: already installed in CI (`Install postgresql-client`
  step) and in the runtime image. No change.
- **`PGPASSWORD`**: the resurrected psql path sets it inline in the
  shell command (as `77bcc36` did). Unchanged.
- **Lazy wire connection**: `connection.pl` does not connect on import,
  only on first `pg_query/3` call. A `PG_BACKEND=psql` run never
  opens a TCP socket.
- **`probe.pl`**: untouched. Reads via `connection.pl`; not affected
  by the toggle.
- **Worker entrypoint**: no changes. Repositories expose the same
  public arities regardless of backend.

## 7. Documentation

Add a short note in `doc/` covering:

- `PG_BACKEND` values + the default
- The list of features explicitly mirrored on the psql path
  (`clean_text/2`, `exists_by_uri_t/3` via `count_matching_records/2`)
- The list of post-`b2d726e` features **not** mirrored — §8 below

## 8. Known asymmetries (wire-only, not ported to psql)

The toggle is an escape hatch, not a perfect equivalence. Features
landed after `b2d726e` that are **not** mirrored in the psql clauses:

- `record_pg_query_failure/3` — wire-only by design; logs the
  bind-param API failures.
- Scryer SIGSEGV catch-and-continue in `getAuthorFeed`
  (commit `48431e9`) — wire-only because the failure mode it works
  around is specific to `pg_query/3`.
- The `extract_lookup_ust_id/2` regression patch shape — psql clause
  reproduces the *behaviour* via a follow-up `SELECT`, but the wire
  path's specific reply-tag handling is not relevant.

Other post-`b2d726e` commits (text cleaning, dedup-by-URI) **are**
mirrored, per §4.

## 9. Out of scope

- Adapting the OLD `repository_dcgs:rows//1` to CSV. We chose the
  separator fix (`-A -F '|'`) instead — smaller surface, no DCG churn.
- Removing the wire path. Both paths coexist permanently until a
  follow-up decision retires one.
- Connection pooling, server-side prepare-cache, or any cross-cutting
  perf work. Out of scope for the toggle.
