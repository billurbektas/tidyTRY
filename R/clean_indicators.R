# floraveg.eu base URL for direct downloads
.floraveg_base_url <- "https://files.ibot.cas.cz/cevs/downloads/floraveg/"

# Known download URLs
.floraveg_urls <- list(
  ellenberg_disturbance = paste0(.floraveg_base_url, "Ellenberg_disturbance.xlsx"),
  dispersal_v1 = paste0(.floraveg_base_url, "Lososova_et_al_2023_Dispersal.xlsx"),
  dispersal_v2 = paste0(.floraveg_base_url, "Lososova_et_al_2023_Dispersal_version2_2024-06-14.xlsx"),
  ellenberg_only = paste0(.floraveg_base_url, "Indicator_values_Tichy_et_al%202022-11-29.xlsx"),
  disturbance_only = paste0(.floraveg_base_url, "disturbance_indicator_values.xlsx"),
  life_form = paste0(.floraveg_base_url, "Life_form.xlsx")
)


#' Download data from floraveg.eu
#'
#' Downloads ecological indicator and trait datasets from
#' [floraveg.eu](https://floraveg.eu/download/). No registration required.
#'
#' @param dataset Name of the dataset to download. One of:
#'   - `"ellenberg_disturbance"` (default): combined Ellenberg indicator
#'     values and disturbance classes
#'   - `"dispersal"`: seed dispersal data (Lososova et al. 2023, version 2)
#'   - `"dispersal_v1"`: seed dispersal data (version 1)
#'   - `"ellenberg"`: Ellenberg indicator values only
#'   - `"disturbance"`: disturbance indicator values only
#'   - `"life_form"`: life form classifications
#' @param dest_dir Directory to save the downloaded file. Defaults to a
#'   temporary directory. Set to a permanent path to cache the file.
#' @param overwrite Logical. Re-download if the file already exists?
#'   Defaults to `FALSE`.
#'
#' @return The file path of the downloaded file (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' # Download to a temp directory
#' f <- download_floraveg("ellenberg_disturbance")
#' indicators <- read_indicators(f, species = my_species)
#'
#' # Download and cache permanently
#' f <- download_floraveg("dispersal", dest_dir = "data/")
#' }
download_floraveg <- function(dataset = c("ellenberg_disturbance", "dispersal",
                                          "dispersal_v1", "ellenberg",
                                          "disturbance", "life_form"),
                              dest_dir = tempdir(),
                              overwrite = FALSE) {
  dataset <- match.arg(dataset)

  # Map "dispersal" to the v2 URL
  url_key <- if (dataset == "dispersal") "dispersal_v2" else dataset
  # Map "ellenberg" to the ellenberg_only URL
  if (dataset == "ellenberg") url_key <- "ellenberg_only"
  if (dataset == "disturbance") url_key <- "disturbance_only"

  url <- .floraveg_urls[[url_key]]
  filename <- basename(sub("%20", " ", url))
  dest_file <- file.path(dest_dir, filename)

  if (file.exists(dest_file) && !overwrite) {
    cli::cli_inform("File already exists: {.path {dest_file}} (use {.code overwrite = TRUE} to re-download)")
    return(invisible(dest_file))
  }

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  cli::cli_inform("Downloading {.val {dataset}} from floraveg.eu...")
  utils::download.file(url, dest_file, mode = "wb", quiet = FALSE)
  cli::cli_inform("Saved to {.path {dest_file}}")

  invisible(dest_file)
}


#' Read and clean Ellenberg indicator values
#'
#' Reads Ellenberg indicator values and disturbance classes from a floraveg.eu
#' export (Excel file), resolves taxonomy, and matches to your species list.
#' Can download the data automatically if no file is provided.
#'
#' @param file Path to the Ellenberg/disturbance Excel file (`.xlsx`).
#'   If `NULL`, the file is downloaded automatically from floraveg.eu
#'   using [download_floraveg()].
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
#' If `file = NULL`, the combined Ellenberg + disturbance dataset is
#' downloaded to a temporary directory. To cache the file permanently,
#' use [download_floraveg()] first and pass the path.
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
#' # Auto-download and clean
#' indicators <- read_indicators(species = my_species)
#'
#' # Or provide a local file
#' indicators <- read_indicators(
#'   file = "data/Ellenberg_disturbance.xlsx",
#'   species = my_species
#' )
#' }
read_indicators <- function(file = NULL,
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

  # Auto-download if no file provided
  if (is.null(file)) {
    file <- download_floraveg("ellenberg_disturbance")
  }

  if (!file.exists(file)) {
    cli::cli_abort("File not found: {.path {file}}")
  }

  # Normalize species input and resolve YOUR species taxonomy
  sp_df <- .normalize_species_input(species, resolve_method = resolve_method, ...)

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

  # Add extra manual matches
  if (!is.null(extra_matches)) {
    sp_df <- dplyr::bind_rows(sp_df, extra_matches |>
      dplyr::rename(species_original = species_source, species_resolved = species_TNRS))
  }

  # Match: look up your resolved species names in the source data
  # Try matching on both original and resolved names
  result <- sp_df |>
    dplyr::left_join(raw, by = c("species_resolved" = "species_source")) |>
    dplyr::filter(!dplyr::if_all(dplyr::all_of(available), is.na))

  # For species not matched via resolved name, try original name
  unmatched <- sp_df |>
    dplyr::filter(!.data$species_original %in% result$species_original) |>
    dplyr::left_join(raw, by = c("species_original" = "species_source")) |>
    dplyr::filter(!dplyr::if_all(dplyr::all_of(available), is.na))

  result <- dplyr::bind_rows(result, unmatched)

  # Select relevant columns
  result <- result |>
    dplyr::select(
      species_original = "species_original",
      species_resolved = "species_resolved",
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
  result <- .handle_duplicates(result, "species_original", available)

  cli::cli_inform("Matched indicators for {nrow(result)} species.")
  result
}


#' Read and clean dispersal trait data
#'
#' Reads seed dispersal data from a Lososova et al. (2023) Excel export,
#' resolves taxonomy, and matches to your species list. Can download the
#' data automatically if no file is provided.
#'
#' @param file Path to the dispersal Excel file (`.xlsx`). If `NULL`,
#'   the file is downloaded automatically from floraveg.eu using
#'   [download_floraveg()].
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
#' # Auto-download and clean
#' dispersal <- read_dispersal(species = my_species)
#'
#' # Or provide a local file
#' dispersal <- read_dispersal(
#'   file = "data/Lososova_et_al_2023_Dispersal.xlsx",
#'   species = my_species
#' )
#' }
read_dispersal <- function(file = NULL,
                           species,
                           species_col = "Taxon",
                           resolve_method = "tnrs",
                           extra_matches = NULL,
                           ...) {
  rlang::check_installed("readxl", reason = "to read Excel files")

  # Auto-download if no file provided
  if (is.null(file)) {
    file <- download_floraveg("dispersal")
  }

  if (!file.exists(file)) {
    cli::cli_abort("File not found: {.path {file}}")
  }

  # Normalize species input and resolve YOUR species taxonomy
  sp_df <- .normalize_species_input(species, resolve_method = resolve_method, ...)

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

  # Add extra manual matches
  if (!is.null(extra_matches)) {
    sp_df <- dplyr::bind_rows(sp_df, extra_matches |>
      dplyr::rename(species_original = species_source, species_resolved = species_TNRS))
  }

  # Match: look up your resolved species names in the source data
  result <- sp_df |>
    dplyr::left_join(raw, by = c("species_resolved" = "species_source")) |>
    dplyr::filter(!dplyr::if_all(dplyr::all_of(available_cols), is.na))

  # For species not matched via resolved name, try original name
  unmatched <- sp_df |>
    dplyr::filter(!.data$species_original %in% result$species_original) |>
    dplyr::left_join(raw, by = c("species_original" = "species_source")) |>
    dplyr::filter(!dplyr::if_all(dplyr::all_of(available_cols), is.na))

  result <- dplyr::bind_rows(result, unmatched)

  # Select and rename columns
  result <- result |>
    dplyr::select(
      species_original = "species_original",
      species_resolved = "species_resolved",
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
  result <- .handle_duplicates(result, "species_original", short_names)

  cli::cli_inform("Matched dispersal data for {nrow(result)} species.")
  result
}


# Internal: normalize species input to a data frame with species_original + species_resolved.
# If a character vector is provided, resolves taxonomy first.
# If a data frame (from resolve_species()) is provided, uses it directly.
.normalize_species_input <- function(species, resolve_method = "tnrs", ...) {
  if (is.data.frame(species)) {
    # Support resolve_species() output naming
    if ("submitted_name" %in% names(species) && "accepted_name" %in% names(species)) {
      return(tibble::tibble(
        species_original = species$submitted_name,
        species_resolved = species$accepted_name
      ))
    }
    # Support custom naming
    if ("species_original" %in% names(species) && "species_resolved" %in% names(species)) {
      return(tibble::tibble(
        species_original = species$species_original,
        species_resolved = species$species_resolved
      ))
    }
    cli::cli_abort(
      "Species data frame must have columns {.val submitted_name}/{.val accepted_name} (from resolve_species()) or {.val species_original}/{.val species_resolved}."
    )
  }

  if (is.character(species)) {
    cli::cli_inform("Resolving taxonomy for your species list...")
    resolved <- resolve_species(species, method = resolve_method, ...)
    return(tibble::tibble(
      species_original = resolved$submitted_name,
      species_resolved = resolved$accepted_name
    ))
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
    dplyr::group_by(.data[[species_col]], .data$species_resolved)

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

  dplyr::bind_rows(non_dup, dup_summary)
}
