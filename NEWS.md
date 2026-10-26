# tidyTRY 0.2.1

* `resolve_species(method = "tnrs")` no longer trusts TNRS's `ID` column
  alone. The TNRS service was seen to return a candidate of one name under
  the ID of another (in a batch of 1,032 names, candidates of *Hypochaeris
  maculata* came back under the ID of *Euphorbia pekinensis*, which was then
  reported as a tie and left unresolved; a repeated call returned the
  correct IDs). Each candidate is now checked against its `Name_submitted`;
  rows under a wrong ID are reassigned by name, rows matching no submitted
  name are dropped, and a warning gives both counts. Results of 0.2.0 can be
  wrong for any name affected this way; re-run `resolve_species()`.

# tidyTRY 0.2.0

This release corrects two errors that changed results. Re-run any analysis
that used `resolve_species(method = "tnrs")`, `process_try()`,
`read_indicators()`, `read_dispersal()` or `remove_experiments()` from
version 0.1.0.

## Breaking changes and corrections

* `resolve_species(method = "tnrs")` could return an illegitimate name. It
  used the single "best" match of `TNRS::TNRS()`. For names with homonyms
  every candidate scores 1, and TNRS can rank an *Illegitimate* or *Invalid*
  homonym first. Examples (TNRS 0.3.6, October 2026):
  - "Festuca pratensis" → *Festuca ovina* instead of *Lolium pratense*;
  - "Agrostis alpina" → *Agrostis rupestris* subsp. *rupestris* instead of
    *Alpagrostis alpina*;
  - "Gentiana campestris" → *Gentianella germanica* instead of
    *Gentianella campestris*;
  - "Anemone nemorosa" → *Anemonoides altaica* instead of
    *Anemonoides nemorosa*.

  `resolve_species()` now requests all candidates (`matches = "all"`) and
  chooses among legitimate names only (status "Accepted" or "Synonym"): the
  highest score, then the first source in `sources` (WCVP before WFO), then
  "Accepted" before "Synonym". Ties between accepted names and names without
  a legitimate candidate are returned as `NA` with `needs_review = TRUE` and
  a `review_reason`; a name taken from the fallback source is returned and
  flagged. TNRS's taxonomic status, accepted author, species and family, and
  its warnings are now returned (new columns `taxonomic_status`,
  `accepted_author`, `accepted_species`, `accepted_family`, `n_legitimate`,
  `tnrs_warning`, `needs_review`, `review_reason`). An empty accepted name is
  `NA`. If TNRS returns nothing (for example offline), an error is raised
  instead of an empty result.

* `remove_experiments()` removed whole datasets. Every dataset with a
  DataID 327 or "Treatment" row was removed, whatever the value. In a TRY
  export of about 8 GB, most such rows read "none", "natural environment" or
  "field", so natural observations were removed with experimental ones (81
  of 210 datasets in one export). It now removes *observations*: an
  observation is kept only if every treatment field it carries (DataID 327,
  any `DataName` starting with "Treatment", "Experiment ID") has a clearly
  natural value (`default_natural_values()`). Other values, ambiguous codes
  and empty values count as experimental. Canopy and leaf-position fields
  are not treatment fields. The values that led to removal are attached as
  attribute `"experimental_values"`. `level = "dataset"` gives the old
  behaviour.

* `process_try()` used the resolved accepted names as a read filter. With a
  wrong accepted name this read another species' records (e.g. all
  *Festuca ovina* records for a study species "Festuca pratensis"). It now
  resolves the study names and all TRY names together, selects TRY records
  by `AccSpeciesID`, attaches an accepted name to every row from the
  contributor's `SpeciesName` (TRY's `AccSpeciesName` only as fallback), and
  keeps the rows whose accepted name is a study taxon.

## New features

* `try_species_names()` lists the distinct `AccSpeciesID`, `AccSpeciesName`
  and `SpeciesName` combinations of TRY files in one streaming pass, so the
  TRY names can be resolved before any trait data are read.
* `read_try()` gains `species_ids`, to filter by TRY `AccSpeciesID`.
* `assign_accepted_names()` attaches accepted names to TRY rows, preferring
  the contributor's `SpeciesName` over TRY's `AccSpeciesName`, which can
  merge species a current backbone keeps apart.
* `default_natural_values()` lists the treatment values treated as natural.

## Other changes

* `stats`, `utils` and `grDevices` are declared in Imports; `TNRS` (>= 0.3.6)
  in Suggests.
* Documentation (`man/`) is generated with roxygen2; NAMESPACE imports are
  declared in `R/tidyTRY-package.R`.
* Continuous integration: `R CMD check` runs on GitHub Actions for macOS,
  Windows and Ubuntu.

# tidyTRY 0.1.0

* First version.
