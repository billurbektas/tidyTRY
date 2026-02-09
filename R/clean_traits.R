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
#' all observations from those datasets. In TRY, experimental datasets are
#' flagged by rows where `DataName == "Treatment"`.
#'
#' @param data A data frame of TRY data.
#' @param data_name_col Name of the DataName column. Defaults to `"DataName"`.
#' @param dataset_id_col Name of the DatasetID column. Defaults to `"DatasetID"`.
#'
#' @return The input data frame with experimental datasets removed.
#' @export
remove_experiments <- function(data, data_name_col = "DataName", dataset_id_col = "DatasetID") {
  experiment_ids <- data |>
    dplyr::filter(.data[[data_name_col]] == "Treatment") |>
    dplyr::pull(.data[[dataset_id_col]]) |>
    unique()

  if (length(experiment_ids) > 0) {
    cli::cli_inform("Removing {length(experiment_ids)} experimental dataset{?s} (DatasetID: {experiment_ids}).")
    data <- data |>
      dplyr::filter(!.data[[dataset_id_col]] %in% experiment_ids)
  } else {
    cli::cli_inform("No experimental datasets found.")
  }

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
