SHELL:=/bin/bash

.PHONY: configure create_session create_poste 

COMPOSE_PROJECT_NAME ?= 'org_example_worker'
WORKER ?= 'org.example.worker'
TMP_DIR ?= '/tmp/tmp_${WORKER}'

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

configure: ### Export environments variables from dot env file 
	@. fun.sh && configure

app__bsky__actor__getProfile: ### app.bsky.actor.getProfile
	@/bin/bash -c '. fun.sh && app__bsky__actor__getProfile'

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