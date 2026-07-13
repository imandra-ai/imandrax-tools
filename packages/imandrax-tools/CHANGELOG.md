# Changelog

Versioning scheme: <IMANDRAX_API_VERSION>.<MINOR>.<PATCH>

## [Unreleased]
- FEAT: IDF
- **BREAKING** `imandrax_api.lib` pretty-printer is moved to `imandrax_api_models.pp`
- FEAT: new `imandrax-tools[widget]` optional dependency, providing web widgets for ImandraX API.

## [20.2.1] - 2026-06-29
-  PP: add config knob for summarizing PO tasks (useful for succeded proof cases)

## [20.2.0] - 2026-06-29
- ImandraX API pretty-printer

## [20.1.0] - 2026-06-21

- Migrate goal-state pretty-printing to native Python implementation

## [20.0.0] - 2026-06-19
- bump imandrax-api to v0.20

## [19.0.3] - 2026-04-25
- improve diagnostic formatting: add severity error
- add rule: opaque-attr-on-type-decl

## [19.0.2] - 2026-04-24

- disable general termination proof error diagnostic as it locates incorrect symbol

## [19.0.1] - 2026-04-24

- fix goal-state exception throwing when unavailable

## [19.0.0] - 2026-04-23

- add iml-eval-corpus subpackages
