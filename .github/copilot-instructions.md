# mrmfa Copilot Instructions

## Project purpose
mrmfa is an R package that generates REMIND MFA input data (`*.cs4r`) via `madrat` workflows and `magclass` objects.

Primary entrypoint:
- `fullMFA()` in `R/fullMFA.R`

Core dependencies and concepts:
- `madrat` for `readSource()`, `calcOutput()`, source handling
- `magclass` for multi-dimensional data arrays (`magpie`)
- package-local `calc*`, `read*`, `convert*`, `tool*` functions in `R/`

## Architecture and file roles
- `calc*.R`: produce model-ready parameter outputs (cement/plastic/steel/common)
- `read*.R`: load and shape source-specific raw data
- `convert*.R`: harmonize source data to package conventions
- `tool*.R`: reusable helpers (interpolation, trade balancing, region helpers)
- `R/fullMFA.R`: orchestration of all required `calcOutput()` calls and filenames

## Required calc-function contract
When adding or editing `calc*` functions, return a list with this structure:
- `x`: magclass object (usually from `readSource()` or derived)
- `weight`: weight object or `NULL`
- `unit`: output unit string
- `description`: human-readable description
- `note`: dimensions metadata string, e.g. `dimensions: (Region,Product Application,value)`
- `isocountries`: `TRUE`/`FALSE`

Keep this contract consistent across all `calc*` functions.

## Naming and dimension conventions
- Function prefixes:
  - `calcCe*` cement
  - `calcPl*` plastics
  - `calcSt*` steel
  - `calcCo*` common drivers
- Output filenames in `fullMFA()` should use the corresponding prefixes (`ce_`, `pl_`, `st_`, `co_`).
- Dimension names and labels must match expected REMIND MFA downstream usage exactly.
- Keep `note` dimension declarations synchronized with the actual object dimensions.

## Data-source workflow guidance
- Prefer reading through `readSource("SourceName", ...)` over direct file IO in `calc*` functions.
- Keep source-specific parsing logic in `read*` and transformation logic in `convert*`/`tool*`.
- Document unit conversions inline where performed (for example mm to m conversions).
- If regional aggregation is needed, provide explicit weighting and document it.

## Editing rules for this repo
- Preserve roxygen documentation style used in `R/`.
- Update function docs when changing parameters, units, dimensions, or behavior.
- Avoid introducing ad hoc dimension labels or naming drift.
- Keep hard-coded values limited and clearly justified.
- Prefer deterministic transformations (no hidden randomness).

## Validation checklist after changes
Run the smallest relevant checks first, then broader checks:
1. Targeted execution of affected outputs (via `calcOutput()` / `fullMFA()` section).
2. `make test`
3. `make lint` (or `make lint-all` for broad refactors)
4. `make check` for full package checks

If docs/signatures changed:
- run `make docs` to regenerate `NAMESPACE` and `man/*.Rd`.

## Common pitfalls to avoid
- Returning non-magclass objects in `x`
- Mismatched `note` dimensions vs actual object dims
- Silent unit changes without updating `unit` and docs
- Duplicating source parsing logic across multiple `calc*` files
- Modifying output filenames in `fullMFA()` without checking downstream consumers
