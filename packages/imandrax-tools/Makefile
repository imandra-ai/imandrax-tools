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

.PHONY: build
build:  ## Build
	rm -rf ./dist/imandrax_tools-*
	uv build --no-sources

.PHONY: test-wheel
test-wheel:  ## Test wheel
	uv run --isolated --no-project --python 3.12 --with ./dist/imandrax_tools-*.whl tests/smoke_test.py
	uv run --isolated --no-project --python 3.12 --with ./dist/imandrax_tools-*.whl --with pytest pytest tests

.PHONY: test-source-dist
test-source-dist:  ## Test source distribution
	uv run --isolated --no-project --python 3.12 --with ./dist/imandrax_tools-*.tar.gz tests/smoke_test.py
	uv run --isolated --no-project --python 3.12 --with ./dist/imandrax_tools-*.tar.gz --with pytest pytest tests

.PHONY: pre-release-test
pre-release-test: build  ## Pre-release test (wheel and source distribution)
	make test-wheel
	make test-source-dist

.PHONY: publish-testpypi
publish-testpypi: pre-release-test  ## Publish to test PyPI
	uv publish \
	--publish-url https://test.pypi.org/legacy/ \
	-u __token__ \
	-p $$(gcloud secrets versions access --project imandra-dev --secret pypi-test-imandrax-api-api-token latest) \
	./dist/imandrax_tools-*

.PHONY: publish-pypi
publish-pypi: pre-release-test  ## Publish to PyPI
	uv publish \
	--publish-url https://upload.pypi.org/legacy/ \
	-u __token__ \
	-p $$(gcloud secrets versions access --project imandra-dev --secret pypi-imandrax-api-api-token latest) \
	./dist/imandrax_tools-*
