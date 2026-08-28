# Working on madrat-based packages (mrmfa, mrindustry, mrremind)

These three R packages (`mrmfa`, `mrindustry`, `mrremind`) are **madrat** ("MadRat" = **Ma**y-all-**d**ata-be-**r**eproducible-**a**nd-**t**ractable) input-data-preparation packages for the REMIND model. They share the same framework, conventions, and tooling described below. The framework source lives at `../madrat`; commons/helper packages (`mrcommons`, `mrdrivers`, `mrcommonsenergy`, `mrenergy`, `magpiesets`, `quitte`, `magclass`, `GDPuc`) are also in this workspace or installed.

## The madrat data pipeline

Data prep is split into 5 function *types*. Each user function is `<type><Source|Calc>` and is **called only through its wrapper** — never call `readMySource()` directly, always `readSource("MySource")`. The wrappers handle caching, the madrat search path across packages, sanity checks, and logging.

| Wrapper | User function | Purpose |
|---|---|---|
| `downloadSource("X")` | `downloadX()` | Download raw source data into a source folder. Optional. |
| `readSource("X", subtype, convert=)` | `readX(subtype)` | Read raw files → magclass (magpie) object. Content must match the source verbatim (only format conversion). |
| (part of `readSource`) | `correctX(x, subtype)` | Optional: fix data-quality issues (NAs, duplicates). Often merged into convert instead. |
| (part of `readSource`) | `convertX(x, subtype)` | Bring data to ISO 3166-1 3-digit country level for **all** countries. Omitting convert = data can't be region-aggregated. |
| `calcOutput("Y", aggregate=)` | `calcY()` | Derive/blend outputs. Returns a **list** (see below). May combine several `readSource`/`calcOutput` calls. |
| `retrieveData("Z", rev=)` | `fullZ(rev, dev, ...)` | Orchestrate a whole collection of `calcOutput` calls → packaged tgz for the model. |

Key rules (from madrat's "Coding etiquette"):
- **Always pass the type as a string literal**: `calcOutput("TauTotal")`, not `t <- "TauTotal"; calcOutput(t)`. The framework statically parses these to build the function network.
- **`calcOutput`/`readSource`/`downloadSource` may only be called from within other read/calc/full functions — never from `tool*` functions.**
- `readSource(..., convert = FALSE)` reads without country-fill (use when feeding another calc, or for non-spatial/global data). `calcOutput(..., aggregate = FALSE)` keeps country resolution (use when a calc feeds another calc).

## The `calcOutput` return list

`calc*` functions **must return a list**:
```r
return(list(
  x           = magpieObject,   # REQUIRED: the data
  weight      = NULL,           # REQUIRED: aggregation weight; NULL = sum countries, else weighted mean
  unit        = "tonnes",       # REQUIRED
  description = "…",            # REQUIRED
  # optional, used for sanity checks / metadata:
  note = "dimensions: (Time,Region,value)",
  min = 0, max = 1,             # bounds checked by the wrapper
  isocountries = FALSE          # set when x is NOT ISO-country resolved
))
```
`weight` must have the same regional (and often temporal) structure as `x`. Look at `?calcOutput` for the full list of allowed fields.

## magclass / magpie objects

Data flows as **magpie** objects (package `magclass`): 3 main dims — spatial (usually ISO3 country), temporal (years like `y2020`), data/name. Common ops used throughout: `getItems`/`getYears`/`getNames`/`getSets`, `dimSums`, `collapseDim`, `add_columns`, `as.magpie`, `mbind`. Subsetting: `x["DEU", 2020, ]`, invert with `x["YUG", , , invert = TRUE]`. Tidyverse-style calcs often round-trip through `quitte::madrat_mule()` (store a data.frame inside a magpie for caching) and `as.magpie(df, spatial=, temporal=, datacol=)`.

## Essential `tool*` helpers

madrat/mrcommons provide reusable, side-effect-free helpers (prefix `tool`):
- `toolCountryFill(x, fill=, verbosity=2)` — expand to all ISO countries, filling missing (used at the end of most `convert*`).
- `toolCountryFillBilateral`, `toolISOhistorical(x, mapping=)` — split/merge historical countries (YUG, SCG, BLX…), mapping via `toolGetMapping("ISOhistorical.csv", where="madrat")`.
- `toolGetMapping(...)` — load region/sector mappings (from `madrat`, package `inst/extdata`, or `where="mappingfolder"`).
- `toolAggregate` — aggregate/disaggregate along a mapping.
- `getISOlist()`, `getConfig()`, `setConfig()`, `getSources`.
- These packages also define **local** tools (e.g. mrmfa: `toolInterpolate`, `toolBackcastByReference`, `toolCleanSteelRegions`, `toolBalanceTrade`, `toolMerge2D`; mrindustry: `tool_expand_tibble`, `tool_fix_IEA_data_for_Industry_subsectors`). Reuse these instead of reinventing.

## Naming conventions

- Files: **one function per file**, filename == function name + `.R` (e.g. `calcCoGDP.R`).
- Function name = `<type>` + `<Type>` where Type is the source/output name in CamelCase (`readWorldSteelDatabase`, `convertWorldSteelDatabase`, `calcStProduction`, `fullMFA`).
- **mrmfa subsystem prefixes** on the Type: `Co` = common (GDP/population), `St` = steel, `Ce` = cement, `Pl` = plastic (e.g. `calcStProduction`, `calcCeTrade`, `calcPlLifetime`). Match the prefix of the subsystem you're extending.
- Output file names in `full*` use snake_case with a subsystem prefix and `.cs4r` extension (`st_production.cs4r`, `ce_clinker_ratio.cs4r`).
- A `readX` supporting multiple datasets takes a `subtype` arg and dispatches (often via a `switchboard` named list) with a validating `stop()` for unknown subtypes.

## Roxygen documentation (required on every function)

Every function needs a roxygen header. Observed conventions:
- `#' <Title>` then `#' @description`, `#' @author <Name>`.
- `@param` for each argument; `@return` describing the magpie/list output (calc functions: note it's "in calcOutput format").
- Cross-reference with `\link{...}` / `[fn()]` (with `@md` for markdown-style docs, as in mrindustry).
- `convert*` often reuses read docs via `#' @inherit readX`.
- `calc*`/`full*` that are user-facing get `#' @export`; internal read/convert/tool functions in these packages are typically **not** exported (madrat finds them via its search path, not NAMESPACE). Check the package's existing pattern before adding `@export`.
- **Never hand-edit `NAMESPACE` or `man/*.Rd`** — they are generated by roxygen2.

## `R/madrat.R` (do not remove)

Each package has `R/madrat.R` with `.onAttach`/`.onDetach` calling `madrat::madratAttach`/`madratDetach`, plus redirects of `cat`/`message`/`warning`/`stop` to `vcat(...)`. This routes messages into madrat's log files — keep using base `message`/`warning`/`stop` in code; they resolve to `vcat` automatically.

## Code style & tooling (lucode2)

- Linting via `lucode2::lintrRules()` (config in `.lintr`). `object_name_linter` is disabled, so mixed snake_case/camelCase locals are tolerated, but **prefer camelCase for new code** to match madrat. Pipe style is `auto` (both `%>%` and `|>` accepted; match the file).
- Indent 2 spaces. Keep lines reasonable; wrap long `calcOutput` arg lists. Use `# nolint start/end` sparingly (as in `fullMFA.R`) for intentionally long lines.
- Use the `Makefile` targets (run from the package root):
  - `make build` → `lucode2::buildLibrary()` (bumps version, regenerates README/CITATION/docs, validates). Pass `make build u=3` for update type. **Run this before committing** feature work — the repo commits show `run buildLibrary()`.
  - `make check` → `lucode2::check()` (docs + tests + etiquette). `make test` → `devtools::test()`. `make lint` (changed files) / `make lint-all`. `make format` → `lucode2::autoFormat()`. `make docs` → roxygenize.
- `.buildlibrary` has `enforceVersionUpdate: yes` — version bumps are required; `buildLibrary` handles it. Bump `Version:` and `Date:` in `DESCRIPTION` come from the build tool, not by hand.
- New package dependencies must be added to `DESCRIPTION` (`Imports:`) and referenced via `pkg::fn` or `@importFrom`.

## Testing

Tests use `testthat` under `tests/testthat/`. Current suites are placeholder `skip("dummy test")` files, so there's little existing coverage to mirror — when adding tests, prefer testing `tool*` helpers directly (they're pure) rather than `calc*`/`read*` (which need the madrat config + source data). Running full `calc*`/`read*` requires a configured madrat mainfolder with source data present.

## Practical workflow for adding a new function

1. Decide the type (`read`/`convert`/`calc`/`full`/`tool`) and pick the correct subsystem prefix.
2. Create `R/<functionName>.R`, one function, with a full roxygen header.
3. For `calc*`: return the list with `x/weight/unit/description` (+ `note`, bounds). Get inputs via `readSource`/`calcOutput` (string literals!), aggregate=FALSE when chaining.
4. For `convert*`: end with `toolCountryFill(x, verbosity = 2)`; handle historical country splits with `toolISOhistorical`.
5. Wire outputs into the relevant `full*` function if they belong in the model bundle.
6. `make docs` (regenerate man/NAMESPACE) → `make lint`/`make format` → `make check` → `make build` before committing.
