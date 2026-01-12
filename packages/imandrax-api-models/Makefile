.PHONY: help
help:  ## Show this help (usage: make help)
	@echo "Usage: make [recipe]"
	@echo "Recipes:"
	@awk '/^[a-zA-Z0-9_-]+:.*?##/ { \
		helpMessage = match($$0, /## (.*)/); \
		if (helpMessage) { \
			recipe = $$1; \
			sub(/:/, "", recipe); \
			printf "  \033[36m%-20s\033[0m %s\n", recipe, substr($$0, RSTART + 3, RLENGTH); \
		} \
	}' $(MAKEFILE_LIST)

.PHONY: test
test:  ## Run tests
	uv run --env-file .env pytest .

.PHONY: build
build:  ## Build
	rm -rf ./dist/imandrax_api_models-*
	uv build --no-sources

.PHONY: test-wheel
test-wheel:  ## Run tests on wheel
	uv run --isolated --no-project --with ./dist/imandrax_api_models-*.whl tests/smoke_test.py

.PHONY: test-source-dist
test-source-dist:  ## Run tests on source dist
	uv run --isolated --no-project --with ./dist/imandrax_api_models-*.tar.gz tests/smoke_test.py

.PHONY: pre-release-test
pre-release-test:  ## Run pre-release tests
	make build
	make test-wheel
	make test-source-dist

.PHONY: publish-testpypi
publish-testpypi: pre-release-test  ## Publish to test.pypi.org
	uv publish \
	--publish-url https://test.pypi.org/legacy/ \
	-u __token__ \
	-p $$(gcloud secrets versions access --project imandra-dev --secret pypi-test-imandrax-api-api-token latest) \
	./dist/imandrax_api_models-*

.PHONY: publish-pypi
publish-pypi: pre-release-test  ## Publish to pypi.org
	uv publish \
	--publish-url https://upload.pypi.org/legacy/ \
	-u __token__ \
	-p $$(gcloud secrets versions access --project imandra-dev --secret pypi-imandrax-api-api-token latest) \
	./dist/imandrax_api_models-*
