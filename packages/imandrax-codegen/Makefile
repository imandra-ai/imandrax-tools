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

.PHONY: test-py
test-py:  ## Run tests for Python package
	uv run pytest test/python

.PHONY: pre-release-test
pre-release-test:  ## Run pre-release tests for Python package
	# uv run pytest test/python
	true  # disable in CI because we don't target platform outside of MacOS

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

# Cross-platform binary builds
# ====================

LINUX_BIN_DIR := ./bin-linux
MACOS_BIN_DIR := ./bin-macos

.PHONY: build-linux-bin
build-linux-bin:  ## Build Linux binary using Docker
	@echo "Building Linux binary via Docker..."
	@mkdir -p $(LINUX_BIN_DIR)
	docker run --rm \
		-v $(PWD)/../..:/workspace/imandrax-tools \
		-v $(PWD)/../../../imandrax-api:/workspace/imandrax-api \
		-w /workspace/imandrax-tools/packages/imandrax-codegen \
		ocaml/opam:ubuntu-24.04-ocaml-5.3 \
		bash -c "\
			sudo apt-get update && sudo apt-get install -y pkg-config libgmp-dev zlib1g-dev libprotobuf-dev protobuf-compiler && \
			opam update && \
			opam pin add -n imandrax-api /workspace/imandrax-api && \
			opam pin add -n imandra-proof-system /workspace/imandrax-api && \
			opam install . --deps-only -y && \
			dune build --root=. bin/parse.exe && \
			cp _build/default/bin/parse.exe bin-linux/art_parse.exe"
	@echo "Linux binary built at $(LINUX_BIN_DIR)/art_parse.exe"

.PHONY: build-macos-bin
build-macos-bin:  ## Build MacOS binary (native)
	@echo "Building MacOS binary..."
	@mkdir -p $(MACOS_BIN_DIR)
	dune build bin/parse.exe
	cp _build/default/bin/parse.exe $(MACOS_BIN_DIR)/art_parse.exe
	@echo "MacOS binary built at $(MACOS_BIN_DIR)/art_parse.exe"

.PHONY: test-linux-wheel
test-linux-wheel:  ## Test Linux wheel in Docker
	@echo "Testing Linux wheel import..."
	docker run --rm -v $(PWD):/workspace python:3.12 \
		bash -c "pip install /workspace/dist/*.whl && python -c 'from imandrax_codegen import art_parse; print(art_parse.CODEGEN_EXE_PATH)'"

# Binary names for GitHub releases
LINUX_BIN_NAME := art_parse-linux-x86_64
MACOS_BIN_NAME := art_parse-darwin-arm64

.PHONY: upload-linux-bin
upload-linux-bin:  ## Upload Linux binary to GitHub release (requires RELEASE_TAG)
	@if [ -z "$(RELEASE_TAG)" ]; then echo "Error: RELEASE_TAG is required"; exit 1; fi
	@echo "Uploading Linux binary to release $(RELEASE_TAG)..."
	gh release upload $(RELEASE_TAG) $(LINUX_BIN_DIR)/art_parse.exe#$(LINUX_BIN_NAME) --clobber

.PHONY: upload-macos-bin
upload-macos-bin:  ## Upload MacOS binary to GitHub release (requires RELEASE_TAG)
	@if [ -z "$(RELEASE_TAG)" ]; then echo "Error: RELEASE_TAG is required"; exit 1; fi
	@echo "Uploading MacOS binary to release $(RELEASE_TAG)..."
	gh release upload $(RELEASE_TAG) $(MACOS_BIN_DIR)/art_parse.exe#$(MACOS_BIN_NAME) --clobber

.PHONY: upload-binaries
upload-binaries: upload-linux-bin upload-macos-bin  ## Upload all binaries to GitHub release
