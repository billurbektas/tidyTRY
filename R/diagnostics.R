#' Trait availability matrix
#'
#' Creates a species-by-trait matrix showing the number of observations
#' available for each combination. Useful for identifying gaps in trait
#' coverage across species.
#'
#' @param data A data frame of cleaned TRY data.
#' @param species_col Name of the species column. Defaults to `"AccSpeciesName"`.
#' @param trait_col Name of the trait column. Defaults to `"TraitName"`.
#'
#' @return A wide tibble with species as rows and traits as columns,
#'   values are observation counts (0 for missing combinations).
#' @export
#'
#' @importFrom tidyr pivot_wider
trait_availability <- function(data,
                               species_col = "AccSpeciesName",
                               trait_col = "TraitName") {
  out <- data |>
    dplyr::filter(!is.na(.data[[species_col]]) & !is.na(.data[[trait_col]])) |>
    dplyr::group_by(.data[[species_col]], .data[[trait_col]]) |>
    dplyr::summarize(count = dplyr::n(), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = dplyr::all_of(trait_col),
      values_from = "count",
      values_fill = 0
    )

  cli::cli_inform(
    "Trait availability matrix: {nrow(out)} species x {ncol(out) - 1} trait{?s}."
  )
  out
}


#' Trait summary statistics
#'
#' Computes per-species, per-trait summary statistics including count, mean,
#' standard deviation, median, minimum, and maximum.
#'
#' @param data A data frame of cleaned TRY data.
#' @param species_col Name of the species column. Defaults to `"AccSpeciesName"`.
#' @param trait_col Name of the trait column. Defaults to `"TraitName"`.
#' @param value_col Name of the numeric value column. Defaults to `"StdValue"`.
#'
#' @return A tibble with columns for species, trait, n, mean, sd, median, min, max.
#' @export
#'
#' @importFrom stats median sd
trait_summary <- function(data,
                          species_col = "AccSpeciesName",
                          trait_col = "TraitName",
                          value_col = "StdValue") {
  data |>
    dplyr::filter(
      !is.na(.data[[species_col]]) &
        !is.na(.data[[trait_col]]) &
        !is.na(.data[[value_col]])
    ) |>
    dplyr::group_by(.data[[species_col]], .data[[trait_col]]) |>
    dplyr::summarize(
      n = dplyr::n(),
      mean = mean(.data[[value_col]], na.rm = TRUE),
      sd = stats::sd(.data[[value_col]], na.rm = TRUE),
      median = stats::median(.data[[value_col]], na.rm = TRUE),
      min = min(.data[[value_col]], na.rm = TRUE),
      max = max(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    )
}
