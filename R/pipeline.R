#' Process TRY data: full pipeline
#'
#' A convenience wrapper that runs the full tidyTRY pipeline: read files,
#' resolve taxonomy, remove experiments, split traits, clean quantitative
#' data, rename traits, and extract coordinates.
#'
#' Each step is independently callable for advanced use cases. This function
#' is for the common 80% workflow.
#'
#' @param files Character vector of file paths, or a directory containing
#'   TRY `.txt` files.
#' @param species Character vector of species names to filter on. With
#'   `resolve_taxonomy = TRUE`, the study names and all TRY names are
#'   resolved together; TRY records are selected by `AccSpeciesID` and kept
#'   when their resolved name is one of the study taxa.
#' @param qualitative_ids Optional integer vector of TraitIDs that are
#'   qualitative. If `NULL`, all traits are treated as quantitative.
#' @param trait_map Optional trait name mapping (from [trait_map()],
#'   [read_trait_map()], or [default_trait_map()]). If `NULL`, no renaming
#'   is done.
#' @param resolve_taxonomy Logical. Resolve species names? Defaults to `TRUE`.
#' @param resolve_method Taxonomy resolution method (`"tnrs"` or `"worldflora"`).
#'   Only used if `resolve_taxonomy = TRUE`.
#' @param max_error_risk Maximum error risk for quantitative trait filtering.
#'   Defaults to 4.
#' @param extract_location Logical. Extract latitude/longitude from data?
#'   Defaults to `TRUE`.
#' @param chunk_size Rows per chunk for reading large files. Defaults to 100,000.
#' @param ... Additional arguments passed to [resolve_species()].
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{quantitative}{Cleaned quantitative trait data.}
#'     \item{qualitative}{Qualitative trait data (empty if no qualitative IDs specified).}
#'     \item{trait_info}{Data frame of TraitID/TraitName pairs found in the data.}
#'     \item{taxonomy}{Species name resolution results for the study names and
#'       all TRY names (NULL if `resolve_taxonomy = FALSE`). Rows of the
#'       trait tables carry `accepted_name` and `name_source` from
#'       [assign_accepted_names()].}
#'     \item{coordinates}{Extracted coordinates (NULL if `extract_location = FALSE`).}
#'     \item{diagnostics}{Trait availability matrix.}
#'     \item{units}{Data frame of trait names and their units.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' result <- process_try(
#'   files = "data/try",
#'   species = c("Papaver rhoeas", "Centaurea cyanus"),
#'   trait_map = default_trait_map(),
#'   resolve_method = "tnrs"
#' )
#'
#' # Access results
#' result$quantitative
#' result$diagnostics
#' }
process_try <- function(files,
                        species,
                        qualitative_ids = NULL,
                        trait_map = NULL,
                        resolve_taxonomy = TRUE,
                        resolve_method = "tnrs",
                        max_error_risk = 4,
                        extract_location = TRUE,
                        chunk_size = 100000L,
                        ...) {
  # Step 1: Resolve the study names and the TRY names together
  taxonomy <- NULL
  if (resolve_taxonomy) {
    cli::cli_rule("Step 1: Resolving taxonomy")
    try_names <- try_species_names(files, chunk_size = chunk_size)
    taxonomy <- resolve_species(
      unique(c(species, try_names$SpeciesName, try_names$AccSpeciesName)),
      method = resolve_method, ...
    )
    lookup <- stats::setNames(taxonomy$accepted_name, taxonomy$submitted_name)
    study <- unique(stats::na.omit(unname(lookup[species])))
    # TRY species whose contributor or TRY name resolves to a study taxon.
    # Reading by ID means a resolved name is never used as a filter.
    hit <- unname(lookup[try_names$SpeciesName]) %in% study |
      unname(lookup[try_names$AccSpeciesName]) %in% study
    species_ids <- unique(try_names$AccSpeciesID[hit])
    unresolved <- species[is.na(lookup[species])]
    if (length(unresolved)) {
      cli::cli_inform("{length(unresolved)} study name{?s} not resolved; matched by exact name instead.")
    }
  }

  # Step 2: Read and filter data
  cli::cli_rule("Step 2: Reading TRY data")
  if (resolve_taxonomy) {
    data <- read_try(files, species = if (length(unresolved)) unresolved else NULL,
                     species_ids = species_ids, chunk_size = chunk_size)
  } else {
    data <- read_try(files, species = species, chunk_size = chunk_size)
  }

  # Fix ALL-CAPS species names (TRY quirk for hyphenated epithets)
  n_caps <- sum(grepl("^[A-Z][A-Z]", data$AccSpeciesName), na.rm = TRUE)
  if (n_caps > 0) {
    cli::cli_inform("Fixing {n_caps} all-caps AccSpeciesName entr{?y/ies}.")
    data$AccSpeciesName <- fix_species_case(data$AccSpeciesName)
  }

  if (resolve_taxonomy) {
    # An ID can hold several species; keep only the rows of study taxa
    data <- assign_accepted_names(data, taxonomy)
    keep <- data$accepted_name %in% study |
      data$SpeciesName %in% unresolved | data$AccSpeciesName %in% unresolved
    cli::cli_inform("Keeping {sum(keep)} of {nrow(data)} row{?s} that belong to the study taxa.")
    data <- data[keep, , drop = FALSE]
  }

  # Step 3: Get trait info
  cli::cli_rule("Step 3: Inspecting traits")
  trait_info <- get_trait_info(data)

  # Step 4: Remove experiments
  cli::cli_rule("Step 4: Removing experimental observations")
  data <- remove_experiments(data)

  # Step 4b: Remove duplicates
  cli::cli_rule("Step 4b: Removing duplicate records")
  data <- remove_duplicates(data)

  # Step 5: Split traits
  cli::cli_rule("Step 5: Splitting traits")
  split <- split_traits(data, qualitative_ids = qualitative_ids)

  # Step 6: Clean quantitative traits
  cli::cli_rule("Step 6: Cleaning quantitative traits")
  quanti <- clean_quantitative(split$quantitative, max_error_risk = max_error_risk)

  # Step 7: Rename traits (optional)
  if (!is.null(trait_map)) {
    cli::cli_rule("Step 7: Renaming traits")
    quanti <- rename_traits(quanti, map = trait_map)
    if (nrow(split$qualitative) > 0) {
      split$qualitative <- rename_traits(split$qualitative, map = trait_map)
    }
  }

  # Step 8: Extract coordinates (optional)
  coordinates <- NULL
  if (extract_location) {
    cli::cli_rule("Step 8: Extracting coordinates")
    coordinates <- extract_coordinates(data)
  }

  # Step 9: Diagnostics
  cli::cli_rule("Step 9: Computing diagnostics")

  # Extract units before any further filtering
  units <- quanti |>
    dplyr::select(dplyr::all_of(c("TraitName", "UnitName"))) |>
    dplyr::distinct()

  diagnostics <- trait_availability(quanti)

  cli::cli_rule("Done")

  list(
    quantitative = quanti,
    qualitative = split$qualitative,
    trait_info = trait_info,
    taxonomy = taxonomy,
    coordinates = coordinates,
    diagnostics = diagnostics,
    units = units
  )
}
