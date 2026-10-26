#' List the species names in TRY data files
#'
#' Reads only the three name columns of one or more TRY export files
#' (`AccSpeciesID`, `AccSpeciesName`, `SpeciesName`) and returns each
#' distinct combination with its number of rows. This is one streaming pass
#' over the files and returns a few thousand rows even for very large exports,
#' so the TRY names can be resolved with [resolve_species()] before any trait
#' data are read.
#'
#' `AccSpeciesName` is TRY's own accepted name, and `SpeciesName` is the name
#' as recorded by the data contributor. TRY's accepted names can merge species
#' that a current backbone keeps apart: for example, records submitted as
#' *Festuca pratensis* are filed under `AccSpeciesName` "Festuca ovina". So
#' both columns are returned and both should be resolved.
#'
#' @inheritParams read_try
#'
#' @return A tibble with columns `AccSpeciesID`, `AccSpeciesName`,
#'   `SpeciesName` and `n_rows`. All-caps `AccSpeciesName` values are fixed
#'   with [fix_species_case()].
#' @export
#'
#' @examples
#' \dontrun{
#' try_names <- try_species_names("data/try")
#' }
try_species_names <- function(files, chunk_size = 100000L, progress = TRUE) {
  files <- .try_files(files)
  name_cols <- readr::cols_only(
    SpeciesName = readr::col_character(),
    AccSpeciesID = readr::col_integer(),
    AccSpeciesName = readr::col_character()
  )
  out <- purrr::map(files, function(f) {
    cli::cli_inform("Reading species names from {.path {basename(f)}}...")
    readr::read_tsv_chunked(
      f,
      callback = readr::DataFrameCallback$new(function(chunk, pos) {
        dplyr::count(chunk, .data$AccSpeciesID, .data$AccSpeciesName,
                     .data$SpeciesName, name = "n_rows")
      }),
      col_names = .try_header(f),
      skip = 1L,
      chunk_size = chunk_size,
      col_types = name_cols,
      progress = progress,
      show_col_types = FALSE
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(AccSpeciesName = fix_species_case(.data$AccSpeciesName)) |>
    dplyr::summarise(n_rows = sum(.data$n_rows),
                     .by = c("AccSpeciesID", "AccSpeciesName", "SpeciesName"))

  cli::cli_inform(
    "Found {dplyr::n_distinct(out$AccSpeciesID)} TRY species ({nrow(out)} name combinations)."
  )
  out
}


#' Attach accepted names to TRY rows
#'
#' Adds an `accepted_name` column to TRY data from a [resolve_species()]
#' result. The contributor's `SpeciesName` is used first and TRY's own
#' `AccSpeciesName` only when `SpeciesName` could not be resolved, because
#' TRY's accepted names can merge distinct species (see
#' [try_species_names()]).
#'
#' @param data TRY data, as returned by [read_try()].
#' @param taxonomy A [resolve_species()] result covering the TRY names
#'   (typically `resolve_species(c(try_names$SpeciesName, try_names$AccSpeciesName))`).
#'
#' @return `data` with two added columns: `accepted_name`, and `name_source`
#'   (`"SpeciesName"`, `"AccSpeciesName"` or `NA` when neither resolved).
#' @export
assign_accepted_names <- function(data, taxonomy) {
  if (!all(c("submitted_name", "accepted_name") %in% names(taxonomy))) {
    cli::cli_abort("{.arg taxonomy} must be a {.fn resolve_species} result.")
  }
  lookup <- stats::setNames(taxonomy$accepted_name, taxonomy$submitted_name)
  from_species <- unname(lookup[data$SpeciesName])
  from_acc <- unname(lookup[fix_species_case(data$AccSpeciesName)])
  data$accepted_name <- dplyr::coalesce(from_species, from_acc)
  data$name_source <- dplyr::case_when(
    !is.na(from_species) ~ "SpeciesName",
    !is.na(from_acc) ~ "AccSpeciesName",
    TRUE ~ NA_character_
  )
  n_missing <- sum(is.na(data$accepted_name))
  if (n_missing > 0) {
    cli::cli_inform("{n_missing} row{?s} without an accepted name.")
  }
  data
}
