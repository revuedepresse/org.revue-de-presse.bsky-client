SHELL:=/bin/bash

.PHONY: configure create_session create_poste doc doc-setup doc-clean doc-serve

COMPOSE_PROJECT_NAME ?= 'org_example_bsky'
WORKER ?= 'org.example.bsky'
TMP_DIR ?= '/tmp/tmp_${WORKER}'

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

configure: ### Export environments variables from dot env file 
	@. fun.sh && configure

app__bsky__actor__getProfile: ### app.bsky.actor.getProfile
	@/bin/bash -c '. fun.sh && app__bsky__actor__getProfile'

app__bsky__feed__getAuthorFeed: ### app.bsky.feed.getAuthorFeed
	@/bin/bash -c '. fun.sh && app__bsky__feed__getAuthorFeed'

app__bsky__graph__getList: ### app.bsky.graph.getList
	@/bin/bash -c '. fun.sh && app__bsky__graph__getList'

app__bsky__graph__getLists: ### app.bsky.graph.getLists
	@/bin/bash -c '. fun.sh && app__bsky__graph__getLists'

infrastructure__list__count: ### Count list records
	@/bin/bash -c '. fun.sh && infrastructure__list__count'

infrastructure__list__query: ### Query list records
	@/bin/bash -c '. fun.sh && infrastructure__list__query'

infrastructure__list__next_event_id: ### Next list event id
	@/bin/bash -c '. fun.sh && infrastructure__list__next_event_id'

infrastructure__list__next_id: ### Next list id
	@/bin/bash -c '. fun.sh && infrastructure__list__next_id'

infrastructure__list_item__count: ### Count list item records
	@/bin/bash -c '. fun.sh && infrastructure__list_item__count'

infrastructure__list_item__query: ### Query list item records
	@/bin/bash -c '. fun.sh && infrastructure__list_item__query'

infrastructure__list_item__next_id: ### Next list item record id
	@/bin/bash -c '. fun.sh && infrastructure__list_item__next_id'

infrastructure__popularity__count: ### Count popularity records
	@/bin/bash -c '. fun.sh && infrastructure__popularity__count'

infrastructure__popularity__query: ### Query popularity records
	@/bin/bash -c '. fun.sh && infrastructure__popularity__query'

infrastructure__popularity__next_id: ### Next popularity record id
	@/bin/bash -c '. fun.sh && infrastructure__popularity__next_id'

infrastructure__publication__count: ### Count publication records
	@/bin/bash -c '. fun.sh && infrastructure__publication__count'

infrastructure__publication__query: ### Query publication records
	@/bin/bash -c '. fun.sh && infrastructure__publication__query'

infrastructure__publication__next_id: ### Next publication record id
	@/bin/bash -c '. fun.sh && infrastructure__publication__next_id'

infrastructure__publisher__count: ### Count publisher records
	@/bin/bash -c '. fun.sh && infrastructure__publisher__count'

infrastructure__publisher__query: ### Query publisher records
	@/bin/bash -c '. fun.sh && infrastructure__publisher__query'

infrastructure__publisher__next_id: ### Next publisher record id
	@/bin/bash -c '. fun.sh && infrastructure__publisher__next_id'

infrastructure__status__count: ### Count status records
	@/bin/bash -c '. fun.sh && infrastructure__status__count'

infrastructure__status__query: ### Query status records
	@/bin/bash -c '. fun.sh && infrastructure__status__query'

infrastructure__status__next_id: ### Next status record id
	@/bin/bash -c '. fun.sh && infrastructure__status__next_id'

com__atproto__repo__createRecord: ### com.atproto.repo.createRecord
	@/bin/bash -c '. fun.sh && com__atproto__repo__createRecord'

com__atproto__server__createSession: ### com.atproto.server.createSession
	@/bin/bash -c '. fun.sh && com__atproto__server__createSession'

list-endpoints: ### List endpoints from Bluesky API documentation
	@/bin/bash -c '. fun.sh && list_endpoints'

list-namespaces: ### List endpoints namespaces
	@/bin/bash -c '. fun.sh && list_namespaces'

list-accessors: ### List endpoints accessors
	@/bin/bash -c '. fun.sh && list_accessors'

list-api-spec-keys: ### List API spec keys
	@/bin/bash -c '. fun.sh && list_api_spec_keys'

list-api-spec-values: ### List API spec values
	@/bin/bash -c '. fun.sh && list_api_spec_values'

test: ## Test the client
	@/bin/bash -c '. fun.sh && test'

compose-up: ### Start the test Postgres container (waits until healthy)
	@docker compose up -d postgres
	@printf 'Waiting for postgres to become healthy '
	@until [ "$$(docker compose ps --format '{{.Service}} {{.Health}}' 2>/dev/null | awk '$$1=="postgres"{print $$2}')" = "healthy" ]; do printf '.'; sleep 1; done
	@echo ' ok'

compose-down: ### Stop the test Postgres container and drop its volume
	@docker compose down -v

test-scram: compose-up ### Round-trip SCRAM-SHA-256 against the containerized Postgres
	@set -a; . ./.env.test; set +a; \
	scryer-prolog ./tests/pg/scram_test.pl -g 'run_test'

test-pg-query: compose-up ### Round-trip bind-param INSERT/SELECT through connection.pl
	@set -a; . ./.env.test; set +a; \
	scryer-prolog ./tests/pg/pg_query_test.pl -g 'run_test'

test-idempotence: compose-up ### Verify ON CONFLICT (hash) DO NOTHING on the publication table
	@set -a; . ./.env.test; set +a; \
	scryer-prolog ./tests/pg/idempotence_test.pl -g 'run_test'

test-repository-inserts: compose-up ### Exercise repository_status:insert/3 and repository_popularity:do_insert_without_unicity_check/2 end-to-end
	@set -a; . ./.env.test; set +a; \
	scryer-prolog ./tests/pg/repository_inserts_test.pl -g 'run_test'

test-repository-publisher: compose-up ### Exercise repository_publisher:insert/2 (new + duplicate paths) and the $$1::bigint / $$2::bigint bind casts
	@set -a; . ./.env.test; set +a; \
	scryer-prolog ./tests/pg/repository_publisher_test.pl -g 'run_test'

test-repository-list: compose-up ### Exercise repository_list:insert/2 (new path) and the $$2::bigint bind cast on list_id
	@set -a; . ./.env.test; set +a; \
	scryer-prolog ./tests/pg/repository_list_test.pl -g 'run_test'

test-by-indexed-at: compose-up ### Reproducer for the by_indexed_at/2 SIGSEGV under the SELECT/INSERT/SELECT cycle
	@set -a; . ./.env.test; set +a; \
	RUST_BACKTRACE=full RUST_LIB_BACKTRACE=full \
	scryer-prolog ./tests/pg/by_indexed_at_test.pl -g 'run_test'

test-by-indexed-at-integration: ### Integration reproducer: drives by_indexed_at/2 against production rows via .env.local (read-only). Requires the patched scryer-prolog binary to survive.
	@set -a; . ./.env.local; set +a; \
	RUST_BACKTRACE=full RUST_LIB_BACKTRACE=full \
	scryer-prolog ./tests/pg/by_indexed_at_integration_test.pl -g 'run_test'

test-segv-replay: ### Replays /tmp/segv-investigation/last-by-indexed-at.pl against the live wire client. SIGSEGV here means the wire path alone is enough to reproduce.
	@set -a; . ./.env.local; set +a; \
	RUST_BACKTRACE=full RUST_LIB_BACKTRACE=full \
	scryer-prolog ./tests/pg/segv_replay_test.pl -g 'run_test'

test-extract-lookup-ust-id: ### Regression: repository_status:extract_lookup_ust_id/2 rejects malformed lookup replies
	@scryer-prolog ./tests/pg/extract_lookup_ust_id_test.pl -g 'run_test'

test-memoize-arity: ### Regression: getAuthorFeed no longer assertz's wide responses (scryer max_arity = 255)
	@scryer-prolog ./tests/memoize_arity_test.pl -g halt

test-iterate-or-report-failure: ### Regression: silent maplist failure on the feed surfaces as a labelled throw, not a quiet success
	@scryer-prolog ./tests/iterate_or_report_failure_test.pl -g halt

test-feed-capture-replay: ### Hermetic replay of /tmp/segv-investigation/last-feed-pairs.pl through insert_record_args/9
	@scryer-prolog ./tests/feed_capture_replay_test.pl -g halt

probe-prod-auth: ### Read-only auth probe against the DB defined in .env.local
	@set -a; . ./.env.local; set +a; \
	scryer-prolog ./src/infrastructure/pg/probe.pl -g 'run'

doc-setup: ### Clone doclog's own dependencies (teruel, djota) into deps/doclog
	@if [ ! -f deps/doclog/Makefile ]; then \
		echo "Initializing deps/doclog submodule"; \
		git submodule update --init deps/doclog; \
	fi
	@$(MAKE) -C deps/doclog setup

doc: ### Generate HTML documentation from doclog comments into ./doc/html
	@if [ ! -d deps/doclog/teruel ] || [ ! -d deps/doclog/djota ] || [ ! -d deps/doclog/scryer-prolog ]; then \
		$(MAKE) doc-setup; \
	fi
	@mkdir -p doc/html
	@bash deps/doclog/doclog.sh . doc/html

doc-clean: ### Remove generated documentation under doc/html
	@rm -rf doc/html

DOC_PORT ?= 8000

doc-serve: doc ### Rebuild then serve doc/html on http://localhost:$(DOC_PORT)
	@echo "Serving doc/html on http://localhost:$(DOC_PORT)"
	@python3 -m http.server $(DOC_PORT) --directory doc/html
