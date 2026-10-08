# Changelog

Versioning scheme: <IMANDRAX_API_VERSION>.<MINOR>.<PATCH>

## [Unreleased]
- FEAT: `gen_test_cases(..., imandrax_url=...)` - ImandraX at a self-hosted server (or `$IMANDRAX_URL`), where no API key is needed; Imandra's cloud still requires one
- DEPS: bump `imandrax-api-models>=20.12.0` - the connection is resolved by its `get_imandrax_client(url=...)`, not here
- CHANGE: `gen_test_cases` / `gen_counter_example` now honour `$IMANDRAX_URL`, which beats `imandrax_env`; the API key may also come from `~/.config/imandrax/api_key`
- CHANGE: an `imandrax_env` / `$IMANDRAX_ENV` other than 'dev' / 'prod' is a `ValueError`, instead of falling back to prod

## [20.1.0] - 2026-10-05
- **BREAKING** `gen_test_cases`: `decomp_name` is now an optional keyword argument after `lang`; exactly one of `decomp_name` (with optional `other_decomp_kwargs`) or `decomp_plan` must be given
- FEAT: support composite decomp via `gen_test_cases(..., decomp_plan=...)`
- FEAT: `gen_test_cases` accepts `compute_timeout`
- DEPS: bump `imandrax-api-models>=20.8.1`, `iml-query>=0.13`

## [20.0.1] - 2026-06-19
- FIX: sample needs explicit prune and string_results arg since v20

## [20.0.0] - 2026-06-19

- Bump imandrax-api version to v0.20

## [19.0.0] - 2026-03-25

- imandrax-api update: qcheck

## [18.6.1] - 2026-03-23

- Fix duplicated option-lib generation
- Use tree-sitter query for type declaration extraction

## [18.6.0] - 2026-03-20

- Handle infeasible regions

## [18.5.0] - 2026-03-19

- Public API for counter-example source code generation

## [18.4.0] - 2026-03-06

- Return type definition and test declaration separately

## [18.3.0] - 2026-02-24

- Add linux+aarch64 support

## [18.1.4] - 2026-01-19

- Fix MacOS build

## [18.1.3] - 2026-01-19

- Add support for Linux

## [18.1.2] - 2026-01-14

- Make gen-test command reusable in other CLI app

## [18.1.1] - 2026-01-14

- Fixed: missing bundled binary

## [18.1.0] - 2026-01-12

- Relax Python version to 3.12

## [18.0.0] - 2026-01-09

- Initial release
