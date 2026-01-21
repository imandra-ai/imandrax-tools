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

.PHONY: check
check: cicd-format cicd-lint cicd-type-check

.PHONY: cicd-format
cicd-format:
	uvx ruff format . --check

.PHONY: cicd-lint
cicd-lint:
	uvx ruff check . --no-fix --output-format=github

.PHONY: cicd-type-check
cicd-type-check:
	uvx pyright src/ tests/

.PHONY: test
test:  ## Run tests
	uv run pytest -vvv

.PHONY: write-example-trees
write-example-trees:
	uv run scripts/write_tree.py --examples-dir ../iml_examples --write-files

.PHONY: build
build:  ## Build wheel and source distribution
	rm -rf ./dist/iml_query-*
	uv build --no-sources

.PHONY: test-wheel
test-wheel:  ## Run tests on wheel
	uv run --isolated --no-project --with ./dist/iml_query-*.whl tests/smoke_test.py

.PHONY: test-source-dist
test-source-dist:  ## Run tests on source distribution
	uv run --isolated --no-project --with ./dist/iml_query-*.tar.gz tests/smoke_test.py

.PHONY: pre-release-test
pre-release-test:  ## Pre-release test
	make build
	make test-wheel
	make test-source-dist

.PHONY: publish-testpypi
publish-testpypi: pre-release-test  ## Publish to test.pypi.org
	uv publish \
	--publish-url https://test.pypi.org/legacy/ \
	-u __token__ \
	-p $$(gcloud secrets versions access --project imandra-dev --secret pypi-test-imandrax-api-api-token latest) \
	./dist/iml_query-*

.PHONY: publish-pypi
publish-pypi: pre-release-test  ## Publish to pypi.org
	uv publish \
	--publish-url https://upload.pypi.org/legacy/ \
	-u __token__ \
	-p $$(gcloud secrets versions access --project imandra-dev --secret pypi-imandrax-api-api-token latest) \
	./dist/iml_query-*
