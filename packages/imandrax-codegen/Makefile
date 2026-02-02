.PHONY: help
help:  ## Show this help message
	@awk '\
		/^# .+$$/ && !/^# [-=]+$$/ { pending=substr($$0, 3); next } \
		/^# =+$$/ && pending { printf "\033[1;33m=== %s ===\033[0m\n", pending; pending=""; next } \
		/^# -+$$/ && pending { printf "\033[1;33m--- %s ---\033[0m\n", pending; pending=""; next } \
		{ pending="" } \
		/^[a-zA-Z_-]+:.*## / { \
			split($$0, a, ":.*## "); \
			printf "\033[36m%-20s\033[0m %s\n", a[1], a[2] \
		}' $(MAKEFILE_LIST)

TEST_DATA_ART_DIR := test/data/art
TEST_DATA_SIR_DIR := test/data/sir
TEST_DATA_TS_DIR := test/data/ts

export TEST_DATA_ART_DIR
export TEST_DATA_SIR_DIR
export TEST_DATA_TS_DIR

reload-env:  ## Reload .env file
	rm .env
	@touch .env
	@echo "TEST_DATA_ART_DIR=$(TEST_DATA_ART_DIR)" >> .env
	@echo "TEST_DATA_SIR_DIR=$(TEST_DATA_SIR_DIR)" >> .env
	@echo "TEST_DATA_TS_DIR=$(TEST_DATA_TS_DIR)" >> .env

# Test data generation
# ====================

# Clean generated artifacts
.PHONY: clean-artifacts
clean-artifacts:
	rm -rf scripts/gen_model_art/result/*
	rm -rf scripts/gen_fun_decomp_art/result/*
	rm -rf scripts/gen_decl_art/result/*

gen-artifacts: gen-model-art gen-fun-decomp-art gen-decl-art  ## Generate all artifacts

.PHONY: gen-model-art
gen-model-art:
	uv run scripts/gen_model_art

.PHONY: gen-fun-decomp-art
gen-fun-decomp-art:
	uv run scripts/gen_fun_decomp_art

.PHONY: gen-decl-art
gen-decl-art:
	uv run scripts/gen_decl_art

copy-artifacts: copy-model copy-fun-decomp copy-decl  ## Copy artifacts to test data directories

.PHONY: copy-art-model
copy-art-model:
	@echo "Copying model artifacts to test/data/model/..."
	rsync -av --delete scripts/gen_model_art/result/ $(TEST_DATA_ART_DIR)/model/

.PHONY: copy-art-fun-decomp
copy-art-fun-decomp:
	@echo "Copying fun_decomp artifacts to test/data/fun_decomp/..."
	rsync -av --delete scripts/gen_fun_decomp_art/result/ $(TEST_DATA_ART_DIR)/fun_decomp/

.PHONY: copy-art-decl
copy-art-decl:
	@echo "Copying decl artifacts to test/data/decl/..."
	rsync -av --delete scripts/gen_decl_art/result/ $(TEST_DATA_ART_DIR)/decl/

.PHONY: gen-sir
gen-sir:  ## Generate SIR S-expressions from twine data
	@echo "Generating SIR S-expressions..."
	dune exec test/data/gen-sir/gen_sir.exe

.PHONY: gen-ts
gen-ts:  ## Generate TypeScript code from SIR data
	@echo "Generating TypeScript code..."
	dune exec test/data/gen-ts/gen_ts.exe

gen-cram-tests: gen-cram-test-model gen-cram-test-fun-decomp gen-cram-test-decl  ## Generate cram test files

.PHONY: gen-cram-test-model
gen-cram-test-model:
	@echo "Regenerating test_parse_model.t..."
	bash test/e2e/python/cram_test_gen__model.sh > test/e2e/python/test_parse_model.t

.PHONY: gen-cram-test-fun-decomp
gen-cram-test-fun-decomp:
	@echo "Regenerating test_parse_fun_decomp.t..."
	bash test/e2e/python/cram_test_gen__fun_decomp.sh > test/e2e/python/test_parse_fun_decomp.t

.PHONY: gen-cram-test-decl
gen-cram-test-decl:
	@echo "Regenerating test_parse_decl.t..."
	bash test/e2e/python/cram_test_gen__decl.sh > test/e2e/python/test_parse_decl.t

# Cross-platform binary builds
# ====================

BIN_DIR := .bin

.PHONY: clean-binaries
clean-binaries:  ## Clean built binaries
	rm -rf $(BIN_DIR)
	rm -rf _build-linux  # Docker build directory

.PHONY: build-linux-bin
build-linux-bin:  ## Build Linux binary using Docker
	@echo "Building Linux binary via Docker..."
	@mkdir -p $(BIN_DIR)
	docker run --rm --platform linux/amd64 \
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
			dune build --root=. --build-dir=_build-linux bin/parse.exe && \
			cp _build-linux/default/bin/parse.exe .bin/art_parse-linux-x86_64"
	@echo "Linux binary built at $(BIN_DIR)/art_parse-linux-x86_64"

.PHONY: build-macos-bin
build-macos-bin:  ## Build MacOS binary (native)
	@echo "Building MacOS binary..."
	@mkdir -p $(BIN_DIR)
	dune build bin/parse.exe
	cp ../../_build/default/packages/imandrax-codegen/bin/parse.exe $(BIN_DIR)/art_parse-darwin-arm64
	@echo "MacOS binary built at $(BIN_DIR)/art_parse-darwin-arm64"

.PHONY: build-binaries
build-binaries: build-macos-bin build-linux-bin  ## Build all binaries

.PHONY: test-linux-wheel
test-linux-wheel: build-py-linux  ## Build and test Linux wheel in Docker
	@echo "Testing Linux wheel import..."
	docker run --rm --platform linux/amd64 -v $(PWD):/workspace python:3.12 \
		bash -c "pip install /workspace/dist/imandrax_codegen-*-manylinux*.whl && python -c 'from imandrax_codegen import art_parse; print(art_parse.CODEGEN_EXE_PATH)'"

# Default release tag for binaries
BINARY_RELEASE_TAG ?= codegen-binaries

# The --clobber flag in the Makefile means:
# - First upload: Creates art_parse-linux-x86_64 and art_parse-darwin-arm64 in the
# codegen-binaries release
# - Subsequent uploads: Overwrites the existing binaries with the new ones
.PHONY: upload-linux-bin
upload-linux-bin:  ## Upload Linux binary to GitHub release (uses BINARY_RELEASE_TAG, default: codegen-binaries)
	@echo "Uploading Linux binary to release $(BINARY_RELEASE_TAG)..."
	gh release upload $(BINARY_RELEASE_TAG) $(BIN_DIR)/art_parse-linux-x86_64 --clobber

.PHONY: upload-macos-bin
upload-macos-bin:  ## Upload MacOS binary to GitHub release (uses BINARY_RELEASE_TAG, default: codegen-binaries)
	@echo "Uploading MacOS binary to release $(BINARY_RELEASE_TAG)..."
	gh release upload $(BINARY_RELEASE_TAG) $(BIN_DIR)/art_parse-darwin-arm64 --clobber

.PHONY: upload-binaries
upload-binaries: upload-linux-bin upload-macos-bin  ## Upload all binaries to GitHub release

# Py
# ====================

.PHONY: build-py-macos
build-py-macos: build-macos-bin  ## Build macOS wheel locally
	@echo "Building macOS wheel..."
	rm -rf ./dist/imandrax_codegen-*
	cp $(BIN_DIR)/art_parse-darwin-arm64 python/imandrax_codegen/art_parse.exe
	uv build --wheel
	@echo "Retagging wheel for macOS..."
	uv tool run wheel tags --platform-tag macosx_11_0_arm64 --remove dist/*.whl
	@echo "macOS wheel built: $$(ls dist/*.whl)"

.PHONY: build-py-linux
build-py-linux:  ## Build Linux wheel locally
	@echo "Building Linux wheel..."
	rm -rf ./dist/imandrax_codegen-*
	cp $(BIN_DIR)/art_parse-linux-x86_64 python/imandrax_codegen/art_parse.exe
	uv build --wheel
	@echo "Retagging wheel for Linux..."
	uv tool run wheel tags --platform-tag manylinux_2_17_x86_64 --remove dist/*.whl
	@echo "Linux wheel built: $$(ls dist/*.whl)"

.PHONY: test-py
test-py:  ## Run tests for Python package
	uv run pytest test/python

.PHONY: pre-release-test-linux
pre-release-test-linux:  ## Run pre-release tests for Linux Python wheel
	uv run --no-project --with "$$(ls ./dist/imandrax_codegen-*manylinux*.whl)" --with pytest --with inline-snapshot --with pyyaml pytest test/python

.PHONY: pre-release-test-macos
pre-release-test-macos:  ## Run pre-release tests for macOS Python wheel
	uv run --no-project --with "$$(ls ./dist/imandrax_codegen-*macos*.whl)" --with pytest --with inline-snapshot --with pyyaml pytest test/python

.PHONY: publish-testpypi
publish-testpypi:  ## Publish to test.pypi.org
	uv publish \
	--publish-url https://test.pypi.org/legacy/ \
	-u __token__ \
	-p $$(gcloud secrets versions access --project imandra-dev --secret pypi-test-imandrax-api-api-token latest) \
	./dist/imandrax_codegen-*

.PHONY: publish-pypi
publish-pypi:  ## Publish to pypi.org
	uv publish \
	--publish-url https://upload.pypi.org/legacy/ \
	-u __token__ \
	-p $$(gcloud secrets versions access --project imandra-dev --secret pypi-imandrax-api-api-token latest) \
	./dist/imandrax_codegen-*
