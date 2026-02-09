# Changelog

Versioning scheme: <IMANDRAX_API_VERSION>.<MINOR>.<PATCH>

## [Unreleased]

## [18.1.0] - 2026-02-09

- Added
  - artifact related `pydantic.BaseModel`s
  - `list_artifacts` and `get_artifact_zip` endpoints for extended client
  - extract error messages for internal error
  - opt-in goal-state pretty printing binary wrapper

## [18.0.0] - 2025-12-15

- Changed
  - upgrade to `imandrax-api==0.18.0`
- Added
  - New models: `GetDeclsReq`, `GetDeclsRes`, `DeclWithName`
  - `get_decls()` method in `ImandraXClient` and `ImandraXAsyncClient`

## [1.1.1] - 2025-12-04

- Changed
  - remove task field in formatting
- Fixed
  - allow multiple location information in error

## [1.1.0] - 2025-11-21

- Added
  - context utils for formatting models as LLM-friendly strings
  - extended client with built-in Pydantic validation and tree-sitter utils integration (`iml-query`)

## [1.0.0] - 2025-11-17

- Initial release using artifact decoding utils from `imandrax-api==0.17.3.1`
