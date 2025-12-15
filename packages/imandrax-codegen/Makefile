.PHONY: help gen-artifacts copy-artifacts gen-tests test clean

help:
	@echo "Available targets:"
	@echo "  gen-artifacts    - Generate all test artifacts (model, fun_decomp, decl)"
	@echo "  copy-artifacts   - Copy generated artifacts to test/data/"
	@echo "  gen-tests        - Regenerate cram test files from artifacts"
	@echo "  update-all       - Run all steps: generate, copy, and regenerate tests"
	@echo "  test             - Run all tests"
	@echo "  clean            - Clean generated artifacts"

# Generate all artifacts
gen-artifacts: gen-model-art gen-fun-decomp-art gen-decl-art

gen-model-art:
	@echo "Generating model artifacts..."
	uv run scripts/gen_model_art

gen-fun-decomp-art:
	@echo "Generating fun_decomp artifacts..."
	uv run scripts/gen_fun_decomp_art

gen-decl-art:
	@echo "Generating decl artifacts..."
	uv run scripts/gen_decl_art

# Copy artifacts to test data directories
copy-artifacts: copy-model copy-fun-decomp copy-decl

copy-model:
	@echo "Copying model artifacts to test/data/model/..."
	rsync -av --delete scripts/gen_model_art/result/ test/data/model/

copy-fun-decomp:
	@echo "Copying fun_decomp artifacts to test/data/fun_decomp/..."
	rsync -av --delete scripts/gen_fun_decomp_art/result/ test/data/fun_decomp/

copy-decl:
	@echo "Copying decl artifacts to test/data/decl/..."
	rsync -av --delete scripts/gen_decl_art/result/ test/data/decl/

# Regenerate cram test files
gen-tests: gen-test-model gen-test-fun-decomp

gen-test-model:
	@echo "Regenerating test_parse_model.t..."
	bash test/e2e/cram_test_gen__model.sh > test/e2e/test_parse_model.t

gen-test-fun-decomp:
	@echo "Regenerating test_parse_fun_decomp.t..."
	bash test/e2e/cram_test_gen__fun_decomp.sh > test/e2e/test_parse_fun_decomp.t

# Run all steps in sequence
update-all: gen-artifacts copy-artifacts gen-tests
	@echo "All artifacts generated, copied, and tests regenerated!"
	@echo "Run 'dune test' to execute the tests and 'dune promote' to accept new outputs."

# Run tests
test:
	dune test

# Clean generated artifacts
clean:
	rm -rf scripts/gen_model_art/result/*
	rm -rf scripts/gen_fun_decomp_art/result/*
	rm -rf scripts/gen_decl_art/result/*
