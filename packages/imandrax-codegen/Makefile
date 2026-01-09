.PHONY: help
help:  ## Show this help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

gen-artifacts: gen-model-art gen-fun-decomp-art gen-decl-art  ## Generate all artifacts

.PHONY: gen-model-art
gen-model-art:
	@echo "Generating model artifacts..."
	uv run scripts/gen_model_art

.PHONY: gen-fun-decomp-art
gen-fun-decomp-art:
	@echo "Generating fun_decomp artifacts..."
	uv run scripts/gen_fun_decomp_art

.PHONY: gen-decl-art
gen-decl-art:  ## Generate decl artifacts
	@echo "Generating decl artifacts..."
	uv run scripts/gen_decl_art

copy-artifacts: copy-model copy-fun-decomp copy-decl  ## Copy artifacts to test data directories

.PHONY: copy-model
copy-model:
	@echo "Copying model artifacts to test/data/model/..."
	rsync -av --delete scripts/gen_model_art/result/ test/data/model/

.PHONY: copy-fun-decomp
copy-fun-decomp:
	@echo "Copying fun_decomp artifacts to test/data/fun_decomp/..."
	rsync -av --delete scripts/gen_fun_decomp_art/result/ test/data/fun_decomp/

.PHONY: copy-decl
copy-decl:  ## Copy decl artifacts to test data directories
	@echo "Copying decl artifacts to test/data/decl/..."
	rsync -av --delete scripts/gen_decl_art/result/ test/data/decl/

gen-cram-tests: gen-cram-test-model gen-cram-test-fun-decomp  ## Generate cram test files

.PHONY: gen-cram-test-model
gen-cram-test-model:
	@echo "Regenerating test_parse_model.t..."
	bash test/e2e/cram_test_gen__model.sh > test/e2e/test_parse_model.t

.PHONY: gen-cram-test-fun-decomp
gen-cram-test-fun-decomp:
	@echo "Regenerating test_parse_fun_decomp.t..."
	bash test/e2e/cram_test_gen__fun_decomp.sh > test/e2e/test_parse_fun_decomp.t

update-all:  ## Update all artifacts and regenerate cram test files
	make gen-artifacts
	make copy-artifacts
	make gen-cram-tests
	@echo "All artifacts generated, copied, and tests regenerated!"
	@echo "Run 'dune test' to execute the tests and 'dune promote' to accept new outputs."

# Clean generated artifacts
.PHONY: clean-artifacts
clean:
	rm -rf scripts/gen_model_art/result/*
	rm -rf scripts/gen_fun_decomp_art/result/*
	rm -rf scripts/gen_decl_art/result/*

# Py
# ====================

.PHONY: build-py
build-py:  ## Build Python package
	rm -rf ./dist/imandrax_codegen-*
	uv build --no-sources

.PHONY: pre-release-test
pre-release-test:  ## Run pre-release tests for Python package
	uv run pytest test/python

.PHONY: publish-testpypi
publish-testpypi: pre-release-test  ## Publish to test.pypi.org
	uv publish \
	--publish-url https://test.pypi.org/legacy/ \
	-u __token__ \
	-p $$(gcloud secrets versions access --project imandra-dev --secret pypi-test-imandrax-api-api-token latest) \
	./dist/imandrax_codegen-*

.PHONY: publish-pypi
publish-pypi: pre-release-test  ## Publish to pypi.org
	uv publish \
	--publish-url https://upload.pypi.org/legacy/ \
	-u __token__ \
	-p $$(gcloud secrets versions access --project imandra-dev --secret pypi-imandrax-api-api-token latest) \
	./dist/imandrax_codegen-*
