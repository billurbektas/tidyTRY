#' Get trait information
#'
#' Extracts unique TraitID and TraitName pairs from TRY data. Use this to
#' inspect which traits are available and decide which are qualitative vs
#' quantitative.
#'
#' @param data A data frame of TRY data (as returned by [read_try()]).
#' @param trait_id_col Name of the TraitID column. Defaults to `"TraitID"`.
#' @param trait_name_col Name of the TraitName column. Defaults to `"TraitName"`.
#'
#' @return A tibble with columns `TraitID` and `TraitName`, one row per unique trait.
#' @export
get_trait_info <- function(data, trait_id_col = "TraitID", trait_name_col = "TraitName") {
  out <- data |>
    dplyr::select(dplyr::all_of(c(trait_id_col, trait_name_col))) |>
    dplyr::distinct() |>
    dplyr::filter(!is.na(.data[[trait_id_col]])) |>
    dplyr::arrange(.data[[trait_id_col]])

  cli::cli_inform("Found {nrow(out)} unique trait{?s}.")
  out
}


#' Remove experimental observations
#'
#' Removes TRY observations made under experimental conditions. TRY records
#' growing conditions in "treatment" fields: DataID 327 ("Treatment:
#' Exposition") and further fields whose `DataName` starts with "Treatment"
#' (light, nutrient supply, CO2, temperature, water supply, competition, ...)
#' or reads "Experiment ID". An observation is kept only if every treatment
#' field it carries has a clearly natural value (see
#' [default_natural_values()]); any other value, including ambiguous codes
#' and empty values, marks the observation as experimental, and all its rows
#' are removed. Observations without any treatment field are kept.
#'
#' Before version 0.2.0 this function removed every *dataset* containing a
#' DataID 327 or "Treatment" row, whatever its value. Most such rows say
#' "none" or "natural environment", so natural observations were removed
#' together with experimental ones. That behaviour is still available with
#' `level = "dataset"`.
#'
#' @param data A data frame of TRY data.
#' @param level `"observation"` (default) removes the observations made under
#'   experimental conditions; `"dataset"` reproduces the behaviour before
#'   0.2.0 and removes whole datasets.
#' @param natural_values Character vector of treatment values that mean
#'   natural, untreated conditions. Compared after lower-casing, trimming,
#'   collapsing white space and dropping trailing punctuation. Defaults to
#'   [default_natural_values()]; extend it for the spellings in your export.
#' @param data_id_col,data_name_col,value_col,observation_id_col,dataset_id_col
#'   Column names in `data`.
#'
#' @return The input data frame without experimental observations. The
#'   treatment values that led to removal, with their row counts, are
#'   attached as attribute `"experimental_values"`.
#' @export
remove_experiments <- function(data,
                               level = c("observation", "dataset"),
                               natural_values = default_natural_values(),
                               data_id_col = "DataID",
                               data_name_col = "DataName",
                               value_col = "OrigValueStr",
                               observation_id_col = "ObservationID",
                               dataset_id_col = "DatasetID") {
  level <- match.arg(level)
  is_treatment <- .is_treatment_row(data, data_id_col, data_name_col)

  if (level == "dataset") {
    experiment_ids <- unique(data[[dataset_id_col]][is_treatment])
    if (length(experiment_ids) > 0) {
      cli::cli_inform("Removing {length(experiment_ids)} dataset{?s} with treatment fields (DatasetID: {experiment_ids}).")
      data <- data[!data[[dataset_id_col]] %in% experiment_ids, , drop = FALSE]
    } else {
      cli::cli_inform("No experimental datasets found.")
    }
    return(data)
  }

  values <- .normalize_value(data[[value_col]][is_treatment])
  natural <- values %in% .normalize_value(natural_values)
  experimental_obs <- unique(data[[observation_id_col]][is_treatment][!natural])
  experimental_obs <- experimental_obs[!is.na(experimental_obs)]

  flagged <- tibble::tibble(value = values[!natural]) |>
    dplyr::count(.data$value, sort = TRUE, name = "n_rows")

  drop <- data[[observation_id_col]] %in% experimental_obs
  cli::cli_inform(c(
    "Removing {length(experimental_obs)} experimental observation{?s} ({sum(drop)} row{?s}).",
    "i" = "Treatment values counted as natural: {.val {unique(natural_values)}}."
  ))
  out <- data[!drop, , drop = FALSE]
  attr(out, "experimental_values") <- flagged
  out
}


#' Treatment values that mean natural conditions
#'
#' The values of TRY treatment fields that [remove_experiments()] treats as
#' natural, untreated conditions by default. Only unambiguous values are
#' included; "control", "ambient", "common garden" and codes whose meaning
#' depends on the dataset are not, so observations carrying them are
#' removed. Extend the vector for spelling variants in your export.
#'
#' @return A character vector.
#' @export
default_natural_values <- function() {
  c("none", "no", "no treatment", "untreated", "natural", "natural environment",
    "natural env", "nat env", "natural conditions", "natural vegetation",
    "natural habitat", "natural forest environment", "natural grassland",
    "field", "field conditions", "in situ", "wild")
}


# Internal: rows of TRY treatment fields (DataID 327, DataName "Treatment...",
# "Experiment ID")
.is_treatment_row <- function(data, data_id_col, data_name_col) {
  by_id <- if (data_id_col %in% names(data)) {
    !is.na(data[[data_id_col]]) & data[[data_id_col]] == 327L
  } else {
    rep(FALSE, nrow(data))
  }
  name <- data[[data_name_col]]
  by_name <- !is.na(name) & grepl("^\\s*(treatment|experiment id)", name, ignore.case = TRUE)
  by_id | by_name
}


# Internal: normalise a free-text treatment value for comparison
.normalize_value <- function(x) {
  x <- tolower(trimws(gsub("\\s+", " ", x)))
  x <- sub("[[:punct:][:space:]]+$", "", x)
  x[is.na(x)] <- ""
  x
}


#' Remove duplicate records
#'
#' Removes duplicate trait records flagged by TRY. Per the TRY 6.0 Release
#' Notes, when the same data has been contributed by different sources, the
#' later contribution is marked as a duplicate: its `OrigObsDataID` column
#' points to the original record's `ObsDataID`. Records where
#' `OrigObsDataID != ObsDataID` are duplicates and can be safely removed.
#'
#' @param data A data frame of TRY data.
#' @param obs_data_id_col Name of the ObsDataID column. Defaults to `"ObsDataID"`.
#' @param orig_obs_data_id_col Name of the OrigObsDataID column. Defaults to
#'   `"OrigObsDataID"`.
#'
#' @return The input data frame with duplicate records removed.
#' @export
remove_duplicates <- function(data,
                              obs_data_id_col = "ObsDataID",
                              orig_obs_data_id_col = "OrigObsDataID") {
  if (!orig_obs_data_id_col %in% names(data) || !obs_data_id_col %in% names(data)) {
    cli::cli_warn("Column{?s} {.field {orig_obs_data_id_col}} or {.field {obs_data_id_col}} not found. Skipping duplicate removal.")
    return(data)
  }

  n_before <- nrow(data)

  data <- data |>
    dplyr::filter(
      is.na(.data[[orig_obs_data_id_col]]) |
        .data[[orig_obs_data_id_col]] == .data[[obs_data_id_col]]
    )

  n_removed <- n_before - nrow(data)
  cli::cli_inform("Removed {n_removed} duplicate record{?s}. {nrow(data)} rows remaining.")

  data
}


#' Split traits by type
#'
#' Splits TRY data into quantitative and qualitative subsets based on
#' user-specified qualitative TraitIDs. Traits not listed as qualitative
#' are treated as quantitative.
#'
#' @param data A data frame of TRY data.
#' @param qualitative_ids Integer vector of TraitIDs that are qualitative.
#'   If `NULL`, all traits are treated as quantitative.
#' @param trait_id_col Name of the TraitID column. Defaults to `"TraitID"`.
#'
#' @return A named list with elements `$quantitative` and `$qualitative`,
#'   each a data frame. If `qualitative_ids` is `NULL`, `$qualitative`
#'   is an empty data frame.
#' @export
split_traits <- function(data, qualitative_ids = NULL, trait_id_col = "TraitID") {
  # Remove metadata/covariate rows (TraitID is NA for non-trait rows like

  # coordinates, exposition, elevation, etc.)
  n_meta <- sum(is.na(data[[trait_id_col]]))
  if (n_meta > 0) {
    cli::cli_inform("Dropping {n_meta} metadata/covariate row{?s} (TraitID is NA).")
    data <- data |> dplyr::filter(!is.na(.data[[trait_id_col]]))
  }

  if (is.null(qualitative_ids) || length(qualitative_ids) == 0) {
    cli::cli_inform("No qualitative trait IDs specified. All traits treated as quantitative.")
    return(list(
      quantitative = data,
      qualitative = data[0, ]
    ))
  }

  quali <- data |> dplyr::filter(.data[[trait_id_col]] %in% qualitative_ids)
  quanti <- data |> dplyr::filter(!.data[[trait_id_col]] %in% qualitative_ids)

  cli::cli_inform(
    "Split into {nrow(quanti)} quantitative and {nrow(quali)} qualitative row{?s}."
  )

  list(quantitative = quanti, qualitative = quali)
}


#' Clean quantitative traits
#'
#' Filters quantitative trait data by error risk threshold and optionally
#' removes rows with missing values. In TRY, `ErrorRisk` is a quality
#' measure where lower values indicate more reliable data.
#'
#' @param data A data frame of quantitative TRY data.
#' @param max_error_risk Maximum allowed error risk (exclusive). Rows with
#'   `ErrorRisk >= max_error_risk` are removed. Defaults to 4.
#' @param error_risk_col Name of the ErrorRisk column. Defaults to `"ErrorRisk"`.
#' @param value_col Name of the value column to check for NAs. Defaults to `"StdValue"`.
#' @param remove_na Logical. Remove rows where the value column is `NA`?
#'   Defaults to `TRUE`.
#'
#' @return The filtered data frame.
#' @export
clean_quantitative <- function(data,
                               max_error_risk = 4,
                               error_risk_col = "ErrorRisk",
                               value_col = "StdValue",
                               remove_na = TRUE) {
  n_before <- nrow(data)

  data <- data |>
    dplyr::filter(
      is.na(.data[[error_risk_col]]) | .data[[error_risk_col]] < max_error_risk
    )

  if (remove_na) {
    data <- data |>
      dplyr::filter(!is.na(.data[[value_col]]))
  }

  n_removed <- n_before - nrow(data)
  cli::cli_inform(
    "Removed {n_removed} row{?s} (ErrorRisk >= {max_error_risk} or NA values). {nrow(data)} rows remaining."
  )

  data
}
