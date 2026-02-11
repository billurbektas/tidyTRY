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


#' Remove experimental datasets
#'
#' Identifies datasets that contain treatment/experimental data and removes
#' all observations from those datasets. Per the TRY 6.0 Release Notes,
#' experimental conditions are flagged via DataID 327 ("Exposition"). Falls
#' back to matching `DataName == "Treatment"` for compatibility with older
#' exports.
#'
#' @param data A data frame of TRY data.
#' @param data_id_col Name of the DataID column. Defaults to `"DataID"`.
#' @param data_name_col Name of the DataName column. Defaults to `"DataName"`.
#' @param dataset_id_col Name of the DatasetID column. Defaults to `"DatasetID"`.
#'
#' @return The input data frame with experimental datasets removed.
#' @export
remove_experiments <- function(data,
                               data_id_col = "DataID",
                               data_name_col = "DataName",
                               dataset_id_col = "DatasetID") {
  # Primary: DataID 327 = Exposition / experimental conditions (TRY 6.0 spec)
  has_data_id <- data_id_col %in% names(data)

  if (has_data_id) {
    experiment_ids <- data |>
      dplyr::filter(.data[[data_id_col]] == 327L) |>
      dplyr::pull(.data[[dataset_id_col]]) |>
      unique()
  } else {
    experiment_ids <- integer(0)
  }

  # Fallback: also check DataName == "Treatment" for older exports
  experiment_ids_fallback <- data |>
    dplyr::filter(.data[[data_name_col]] == "Treatment") |>
    dplyr::pull(.data[[dataset_id_col]]) |>
    unique()

  experiment_ids <- unique(c(experiment_ids, experiment_ids_fallback))

  if (length(experiment_ids) > 0) {
    cli::cli_inform("Removing {length(experiment_ids)} experimental dataset{?s} (DatasetID: {experiment_ids}).")
    data <- data |>
      dplyr::filter(!.data[[dataset_id_col]] %in% experiment_ids)
  } else {
    cli::cli_inform("No experimental datasets found.")
  }

  data
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
