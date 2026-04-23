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
check:  ## Format, lint, typecheck
	uvx ruff check src/
	uvx ruff format src/ --check
	uvx ty check src/
	uvx pyright check src/

define get-pypi-latest-ver
$(shell curl -s "https://pypi.org/pypi/$(1)/json" | jq -r '.info.version')
endef

PACKAGES := iml-query imandrax-api-models imandrax-codegen

.PHONY: bump-sub-packages
bump-sub-packages:  ## Bump subpacakges
	uv add --no-sync "iml-query==$(call get-pypi-latest-ver,iml-query)"
	uv add --no-sync "imandrax-api-models==$(call get-pypi-latest-ver,imandrax-api-models)"
	uv add --no-sync "imandrax-api-models[client]==$(call get-pypi-latest-ver,imandrax-api-models)" --optional extended-client
	uv add --no-sync "imandrax-codegen==$(call get-pypi-latest-ver,imandrax-codegen)" --optional codegen
	uv sync --all-extras

CORPUS_SRC := ../knowledge-base/iml-eval-corpus/corpus/
CORPUS_TGT := src/imandrax_tools/iml_eval_corpus/corpus/

.PHONY: sync-corpus
sync-corpus:  ## Sync corpus from knowledge-base/iml-eval-corpus into the package tree (excludes dune files)
	rsync -a --delete \
		--exclude='dune' \
		--exclude='dune.*.inc' \
		--exclude='gen_*.py' \
		--exclude='__pycache__' \
		--exclude='.pytest_cache' \
		--exclude='.ruff_cache' \
		--exclude='_unknown_id_ocaml_stdlib' \
		--exclude='solutions_eval_res.json' \
		$(CORPUS_SRC) \
		$(CORPUS_TGT)
	uvx ruff format $(CORPUS_TGT)
	uvx ruff check $(CORPUS_TGT) --fix

.PHONY: build
build: sync-corpus  ## Build
	rm -rf ./dist/imandrax_tools-*
	uv build --no-sources

.PHONY: test-wheel
test-wheel:  ## Test wheel
	uv run --isolated --no-project --python 3.12 --with ./dist/imandrax_tools-*.whl tests/smoke_test.py
	uv run --isolated --no-project --python 3.12 --with ./dist/imandrax_tools-*.whl --with pytest --with inline-snapshot pytest tests

.PHONY: test-source-dist
test-source-dist:  ## Test source distribution
	uv run --isolated --no-project --python 3.12 --with ./dist/imandrax_tools-*.tar.gz tests/smoke_test.py
	uv run --isolated --no-project --python 3.12 --with ./dist/imandrax_tools-*.tar.gz --with pytest --with inline-snapshot pytest tests

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
