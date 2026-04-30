.PHONY: help
help:  ## Show this help (usage: make help)
	@echo "Usage: make [recipe]"
	@echo "Recipes:"
	@awk '/^[a-zA-Z0-9_.-]+:.*?##/ { \
		helpMessage = match($$0, /## (.*)/); \
		if (helpMessage) { \
			recipe = $$1; \
			sub(/:/, "", recipe); \
			printf "  \033[36m%-20s\033[0m %s\n", recipe, substr($$0, RSTART + 3, RLENGTH); \
		} \
	}' $(MAKEFILE_LIST)


.PHONY: update-cache
update-cache:  ## update imandrax error kind snapshot
	UNTRUSTING=1 uv run pytest main.py --inline-snapshot=fix

.PHONY: gen-error-kind-enum-body
gen-error-kind-enum-body:  ## Generate class body of ErrorKind enum from error kind JSON data
	@uv run --script main.py | LOG_LEVEL=WARNING uv run --script class-of-error-kind-json.py
