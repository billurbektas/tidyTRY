#' Read and clean Ellenberg indicator values
#'
#' Reads Ellenberg indicator values and disturbance classes from a floraveg.eu
#' export (Excel file), resolves taxonomy, and matches to your species list.
#'
#' @param file Path to the Ellenberg/disturbance Excel file (`.xlsx`),
#'   as downloaded from <https://floraveg.eu/download/>.
#' @param species A data frame with columns `species` (original names) and
#'   `species_TNRS` (accepted names), as returned by [resolve_species()]
#'   (use `submitted_name` and `accepted_name`). Alternatively, a character
#'   vector of accepted species names.
#' @param species_col Name of the column in the Excel file containing species
#'   names. Defaults to `"Species-levelName"`.
#' @param indicators Character vector of indicator columns to extract.
#'   Defaults to Ellenberg values and disturbance metrics.
#' @param resolve_method Taxonomy resolution method for the source data.
#'   Defaults to `"tnrs"`.
#' @param extra_matches Optional data frame with columns `species_source`
#'   and `species_TNRS` for manual name overrides (e.g., aggregate taxa
#'   like "Taraxacum sect. Taraxacum" -> "Taraxacum officinale").
#' @param ... Additional arguments passed to [resolve_species()].
#'
#' @return A tibble with species names and cleaned indicator values.
#'
#' @details
#' The function handles common issues with floraveg.eu data:
#' - Values of `"x"` are converted to `NA`
#' - Duplicate species (from subspecies matching) are averaged
#' - Indicator columns are converted to numeric
#'
#' @references
#' Ellenberg values: <https://doi.org/10.1111/jvs.13168>
#'
#' Disturbance classes: <https://onlinelibrary.wiley.com/doi/10.1111/geb.13603>
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # First resolve your species
#' taxonomy <- resolve_species(my_species)
#' sp_df <- data.frame(
#'   species = taxonomy$submitted_name,
#'   species_TNRS = taxonomy$accepted_name
#' )
#'
#' # Then clean indicators
#' indicators <- read_indicators(
#'   file = "data/Ellenberg_disturbance.xlsx",
#'   species = sp_df
#' )
#' }
read_indicators <- function(file,
                            species,
                            species_col = "Species-levelName",
                            indicators = c("Light", "Moisture", "Temperature",
                                           "Continentality", "Nitrogen", "pH",
                                           "Salinity",
                                           "Disturbance.Severity",
                                           "Disturbance.Frequency",
                                           "Grazing.Pressure",
                                           "Mowing.Frequency",
                                           "Soil.Disturbance"),
                            resolve_method = "tnrs",
                            extra_matches = NULL,
                            ...) {
  rlang::check_installed("readxl", reason = "to read Excel files")

  if (!file.exists(file)) {
    cli::cli_abort("File not found: {.path {file}}")
  }

  # Normalize species input
  sp_df <- .normalize_species_input(species)

  # Read source data
  cli::cli_inform("Reading indicators from {.path {basename(file)}}...")
  raw <- readxl::read_excel(file)

  # Check that species_col exists
  if (!species_col %in% names(raw)) {
    cli::cli_abort("Column {.val {species_col}} not found. Available: {.val {names(raw)}}")
  }

  # Remove TaxonName if present, rename species column
  if ("TaxonName" %in% names(raw)) {
    raw <- raw |> dplyr::select(-dplyr::all_of("TaxonName"))
  }
  raw <- raw |> dplyr::rename(species_source = dplyr::all_of(species_col))

  # Check which indicator columns exist
  available <- intersect(indicators, names(raw))
  missing <- setdiff(indicators, names(raw))
  if (length(missing) > 0) {
    cli::cli_warn("Indicator column{?s} not found in data: {.val {missing}}")
  }
  if (length(available) == 0) {
    cli::cli_abort("None of the requested indicator columns found in the data.")
  }

  # Resolve taxonomy from the source dataset
  cli::cli_inform("Resolving taxonomy for source species...")
  source_sp <- unique(raw$species_source)
  source_resolved <- resolve_species(source_sp, method = resolve_method, ...)
  source_match <- tibble::tibble(
    species_source = source_resolved$submitted_name,
    species_TNRS = source_resolved$accepted_name
  )

  # Add extra manual matches
  if (!is.null(extra_matches)) {
    source_match <- dplyr::bind_rows(source_match, extra_matches)
  }

  # Join: source resolved names -> source data -> user species
  result <- source_match |>
    dplyr::left_join(raw, by = "species_source") |>
    dplyr::inner_join(sp_df, by = "species_TNRS")

  # Select relevant columns
  result <- result |>
    dplyr::select(
      dplyr::all_of(c("species", "species_TNRS", "species_source")),
      dplyr::all_of(available)
    )

  # Clean values: "x" -> NA, convert to numeric
  result <- result |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(available),
      ~ {
        val <- ifelse(. == "x", NA, .)
        suppressWarnings(as.numeric(val))
      }
    ))

  # Handle duplicates: average numeric values
  result <- .handle_duplicates(result, "species", available)

  cli::cli_inform("Matched indicators for {nrow(result)} species.")
  result
}


#' Read and clean dispersal trait data
#'
#' Reads seed dispersal data from a Lososova et al. (2023) Excel export,
#' resolves taxonomy, and matches to your species list.
#'
#' @param file Path to the dispersal Excel file (`.xlsx`).
#' @param species A data frame with columns `species` and `species_TNRS`,
#'   or a character vector of accepted species names. See [read_indicators()].
#' @param species_col Name of the species column in the Excel file.
#'   Defaults to `"Taxon"`.
#' @param resolve_method Taxonomy resolution method. Defaults to `"tnrs"`.
#' @param extra_matches Optional data frame for manual name overrides.
#'   See [read_indicators()].
#' @param ... Additional arguments passed to [resolve_species()].
#'
#' @return A tibble with species names and cleaned dispersal traits:
#'   `seed_mass`, `dispersal_mode`, `dispersal_mode_human`,
#'   `dispersal_distance_class`, `dispersal_distance_class_human`.
#'
#' @references
#' Lososova et al. (2023) Dispersal database.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' taxonomy <- resolve_species(my_species)
#' sp_df <- data.frame(
#'   species = taxonomy$submitted_name,
#'   species_TNRS = taxonomy$accepted_name
#' )
#'
#' dispersal <- read_dispersal(
#'   file = "data/Lososova_et_al_2023_Dispersal.xlsx",
#'   species = sp_df
#' )
#' }
read_dispersal <- function(file,
                           species,
                           species_col = "Taxon",
                           resolve_method = "tnrs",
                           extra_matches = NULL,
                           ...) {
  rlang::check_installed("readxl", reason = "to read Excel files")

  if (!file.exists(file)) {
    cli::cli_abort("File not found: {.path {file}}")
  }

  # Normalize species input
  sp_df <- .normalize_species_input(species)

  # Read source data
  cli::cli_inform("Reading dispersal data from {.path {basename(file)}}...")
  raw <- readxl::read_excel(file)

  if (!species_col %in% names(raw)) {
    cli::cli_abort("Column {.val {species_col}} not found. Available: {.val {names(raw)}}")
  }

  raw <- raw |> dplyr::rename(species_source = dplyr::all_of(species_col))

  # Define expected columns and their short names
  col_mapping <- c(
    "Seed mass (mg)" = "seed_mass",
    "Efficient dispersal mode - common" = "dispersal_mode",
    "Efficient dispersal mode - anthropogenic" = "dispersal_mode_human",
    "Dispersal distance class (1-6)" = "dispersal_distance_class",
    "Dispersal distance class (7)" = "dispersal_distance_class_human"
  )

  # Check which columns exist
  available_cols <- intersect(names(col_mapping), names(raw))
  if (length(available_cols) == 0) {
    cli::cli_abort("None of the expected dispersal columns found in the data.")
  }

  missing_cols <- setdiff(names(col_mapping), names(raw))
  if (length(missing_cols) > 0) {
    cli::cli_warn("Dispersal column{?s} not found: {.val {missing_cols}}")
  }

  # Resolve taxonomy from source
  cli::cli_inform("Resolving taxonomy for source species...")
  source_sp <- unique(raw$species_source)
  source_resolved <- resolve_species(source_sp, method = resolve_method, ...)
  source_match <- tibble::tibble(
    species_source = source_resolved$submitted_name,
    species_TNRS = source_resolved$accepted_name
  )

  if (!is.null(extra_matches)) {
    source_match <- dplyr::bind_rows(source_match, extra_matches)
  }

  # Join
  result <- source_match |>
    dplyr::left_join(raw, by = "species_source") |>
    dplyr::inner_join(sp_df, by = "species_TNRS")

  # Select and rename columns
  result <- result |>
    dplyr::select(
      dplyr::all_of(c("species", "species_TNRS", "species_source")),
      dplyr::all_of(available_cols)
    )

  # Rename to short names
  for (old_name in available_cols) {
    result <- result |>
      dplyr::rename(!!col_mapping[old_name] := dplyr::all_of(old_name))
  }

  short_names <- unname(col_mapping[available_cols])

  # Convert numeric columns
  numeric_cols <- intersect(
    c("seed_mass", "dispersal_distance_class", "dispersal_distance_class_human"),
    short_names
  )
  result <- result |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(numeric_cols),
      ~ suppressWarnings(as.numeric(.))
    ))

  # Handle duplicates
  result <- .handle_duplicates(result, "species", short_names)

  cli::cli_inform("Matched dispersal data for {nrow(result)} species.")
  result
}


# Internal: normalize species input to a data frame with species + species_TNRS
.normalize_species_input <- function(species) {
  if (is.character(species)) {
    return(tibble::tibble(species = species, species_TNRS = species))
  }

  if (is.data.frame(species)) {
    # Support both our naming and resolve_species() output naming
    if ("submitted_name" %in% names(species) && "accepted_name" %in% names(species)) {
      return(tibble::tibble(
        species = species$submitted_name,
        species_TNRS = species$accepted_name
      ))
    }
    if ("species" %in% names(species) && "species_TNRS" %in% names(species)) {
      return(tibble::tibble(
        species = species$species,
        species_TNRS = species$species_TNRS
      ))
    }
    cli::cli_abort(
      "Species data frame must have columns {.val species}/{.val species_TNRS} or {.val submitted_name}/{.val accepted_name}."
    )
  }

  cli::cli_abort("{.arg species} must be a character vector or a data frame.")
}


# Internal: handle duplicate species by averaging numerics and taking first character
.handle_duplicates <- function(data, species_col, value_cols) {
  dup_sp <- data[[species_col]][duplicated(data[[species_col]])]

  if (length(dup_sp) == 0) return(data)

  cli::cli_inform("Averaging {length(unique(dup_sp))} duplicated species.")

  non_dup <- data |> dplyr::filter(!.data[[species_col]] %in% dup_sp)

  dup_data <- data |>
    dplyr::filter(.data[[species_col]] %in% dup_sp) |>
    dplyr::group_by(.data[[species_col]], .data$species_TNRS)

  # Average numeric columns, take first non-NA for character columns
  numeric_val_cols <- intersect(
    value_cols,
    names(data)[vapply(data, is.numeric, logical(1))]
  )
  char_val_cols <- intersect(
    value_cols,
    names(data)[vapply(data, is.character, logical(1))]
  )

  dup_summary <- dup_data |>
    dplyr::summarize(
      dplyr::across(dplyr::all_of(numeric_val_cols), ~ mean(., na.rm = TRUE)),
      dplyr::across(dplyr::all_of(char_val_cols), ~ dplyr::first(stats::na.omit(.))),
      .groups = "drop"
    ) |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.numeric),
      ~ ifelse(is.nan(.), NA_real_, .)
    ))

  # Add back species_source as NA for averaged duplicates
  if ("species_source" %in% names(non_dup) && !"species_source" %in% names(dup_summary)) {
    dup_summary$species_source <- NA_character_
  }

  dplyr::bind_rows(non_dup, dup_summary)
}
