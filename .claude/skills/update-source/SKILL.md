---
name: update-source
description: Update a madrat source in mrmfa (read*/convert*/correct*/download* functions and their mapping files) to a new release of the raw data, and re-check everything that depends on it. Use whenever a new data version, release or file layout of any source is placed in madrat_wd/sources/ or the user asks to update a source.
---

# Update a source in mrmfa to a new data release

Applies to any `read<Source>` / `convert<Source>` pair. Source-specific lessons are kept in `references/`
(currently `global-exposure-model.md`, which also shows a worked example). Add a reference file after each update that taught
something non-obvious, and keep this file general.

Rules: follow `.claude/CLAUDE_madrat.md` (never modify madrat_wd, no hand-edited `NAMESPACE` or `man/*.Rd`, no buildlibrary
unless asked). Run Python on the data with `python -I` and keep scripts outside the data directory.

## 1. Orient
1. Find the old and new data in `../madrat_wd/sources/<Source>/` and every place the source is used:
   `grep` for `readSource("<Source>"`, the `read`/`convert`/`correct`/`download` files, mapping files in
   `inst/extdata`, tests, and the calc functions that consume it (and what consumes those, up to `full*`).
2. Read the existing functions completely and list every hard-coded assumption: version string, paths, column names,
   category labels, country or region fixes, filters, NA handling, units, warnings that are accepted. This list drives step 4.

## 2. Audit the release (all changes, not only the breaking ones)
1. Read changelogs, release notes, READMEs and column dictionaries. A top-level changelog may only link to others.
2. Compare old and new release with a short script (run with `python -I`, kept outside the data directory): layout,
   CSV headers, distinct values of low-cardinality columns, and what was added or removed between releases.
   Source-specific audits can live in `references/`.
3. Check explicitly: file layout and names, column names and units, category spellings and case, new or removed
   categories, country/region list and renames, identifier formats, time coverage, duplicated or overlapping rows
   (dimensions that newly split rows, such as settlement type), encoding, and whether totals still add up to the
   source's own summary files.
4. Measure instead of trusting claims in release notes. Skeptically test statements such as "rows are not double counted".

## 3. Look for better ways to calculate what we need
List the quantities the calc functions derive from this source. For each one ask:
- Does the new release provide it directly (new column, file or summary level)? Prefer a direct value over an inferred one.
- Does it offer additional information that improves or validates an existing assumption or inference?
- Does it make an existing workaround, proxy, fill or correction unnecessary?
Validate a direct quantity before relying on it (consistency with related columns, plausibility by country, comparison
with the previous inference); data can be present but unreliable for some regions. Record every candidate as
used / not used and why. Where a change alters results beyond the data update, ask the user before switching.

## 4. Update the code
- `read`: version, paths, columns, spelling and case normalisation, new dimensions. Fail loudly on unknown categories
  (`stop()` with the offending values and their size) rather than dropping rows silently.
- Mapping files: every category in the data must map, and map correctly. Check what each category contains in the
  underlying detail (not only its label). Categories that merge things which belong to different targets must be
  split in code. Test completeness with a testthat test.
- `convert`: re-validate each existing adjustment against the new data instead of assuming it still holds.
  Run it with `verbosity = 2` and compare messages and warnings with the previous release: new warnings need a cause
  (fix the code, or the data issue is documented with a comment), vanished ones mean an adjustment may now be obsolete.
  Check country fills and removals, special-case countries, `replace_non_finite`, unit conversions, and conservation
  of totals through every aggregation.
- Update roxygen (version, citation, URL, changed method), comments that cite numbers from the old release, and docs.
- Add tests for new helper logic (one function per file, `tests/testthat/test-<function>.R`).

## 5. Validate
1. Load with `pkgload::load_all()` and a temporary madrat config (temporary `mainfolder` and `cachefolder`; `sourcefolder`
   and `mappingfolder` point to madrat_wd and are only read). `readSource()` should finish without warnings.
2. Compare read totals with totals the source reports itself.
3. Run every consuming `calcOutput(..., aggregate = FALSE)`; check dimensions, NAs and ranges.
4. Compare old and new outputs (`compareMadratOutputs()` or a scripted diff), and separate the effect of the new data
   from the effect of method changes (run the new data with the old method once).
5. Run the testthat tests of the touched functions.

## 6. Report to the user
Changed columns, categories and values; new and removed warnings and what happened to each adjustment; effect on outputs
(world and country level); direct-data options found (used or not, with the evidence); decisions taken that deviate from
the request or need the user's confirmation; what was not checked (for example `man/` not regenerated, unrelated modified files).
