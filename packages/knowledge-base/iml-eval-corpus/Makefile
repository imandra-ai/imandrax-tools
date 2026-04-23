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

.PHONY: test
test:  ## Test detectors can match repro.iml and won't match solution.iml
	uv run pytest

.PHONY: check-query-module-sig
check-query-module-sig:  ## Check query module sigature
	uvx pyright src/ tests/
	uvx ty check src/ tests/

.PHONY: check
check: test check-query-module-sig  ## All checks
