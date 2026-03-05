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

BATTERIES_SRC := $(shell find batteries -type f)
SKILL_MD_DEPS := $(filter-out skill/SKILL.md, $(wildcard skill/*.md))

skill/extended-prelude: $(BATTERIES_SRC)  ## Copy batteries to extended-prelude
	rsync -a --delete --exclude='tests/' --exclude='README.md' batteries/ $@/

skill/SKILL.md: $(SKILL_MD_DEPS) skill/extended-prelude  ## Update SKILL.md with the latest directory structure
	janet scripts/update-skill-reference.janet

skill: skill/SKILL.md  ## Update skill/