SHELL:=/bin/bash

.PHONY: configure create_session create_poste 

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
