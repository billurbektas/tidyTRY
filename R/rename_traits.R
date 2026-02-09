#' Suggest a trait map from data
#'
#' Inspects the unique trait names in a TRY dataset and prints them for
#' review. Returns a template data frame that can be filled in and used
#' with [rename_traits()].
#'
#' @param data A data frame of TRY data.
#' @param trait_col Name of the trait name column. Defaults to `"TraitName"`.
#'
#' @return A tibble with columns `original` (the TRY trait names) and
#'   `short_name` (empty, for the user to fill in).
#' @export
#'
#' @examples
#' \dontrun{
#' template <- suggest_trait_map(my_data)
#' # Edit the template, then use it:
#' template$short_name <- c("SLA", "LDMC", "N_percent", ...)
#' rename_traits(my_data, template)
#' }
suggest_trait_map <- function(data, trait_col = "TraitName") {
  traits <- sort(unique(data[[trait_col]]))
  traits <- traits[!is.na(traits)]

  cli::cli_inform("Found {length(traits)} unique trait name{?s}:")
  for (tr in traits) {
    cli::cli_inform("
 {tr}")
  }

  tibble::tibble(
    original = traits,
    short_name = rep(NA_character_, length(traits))
  )
}


#' Create a trait name mapping
#'
#' Constructs a trait map where each new short name maps to one or more
#' original TRY trait names. This enables merging related traits (e.g.,
#' multiple SLA definitions) under a single name.
#'
#' @param ... Named arguments where the name is the new short name and the
#'   value is a character vector of original TRY trait names.
#' @param .map An optional existing trait map (data frame or named vector)
#'   to extend.
#'
#' @return An object of class `"try_trait_map"`, which is a tibble with
#'   columns `original` and `short_name`.
#' @export
#'
#' @examples
#' my_map <- trait_map(
#'   SLA = c(
#'     "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded",
#'     "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included"
#'   ),
#'   N_percent = "Leaf nitrogen (N) content per leaf dry mass",
#'   LDMC = "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)"
#' )
trait_map <- function(..., .map = NULL) {
  args <- list(...)

  if (length(args) == 0 && is.null(.map)) {
    cli::cli_abort("Provide at least one mapping (e.g., {.code SLA = 'trait name'}).")
  }

  rows <- list()
  for (short in names(args)) {
    originals <- args[[short]]
    if (!is.character(originals)) {
      cli::cli_abort("Values must be character vectors. Got {.type {originals}} for {.val {short}}.")
    }
    rows[[length(rows) + 1]] <- tibble::tibble(
      original = originals,
      short_name = short
    )
  }

  new_map <- dplyr::bind_rows(rows)

  if (!is.null(.map)) {
    if (inherits(.map, "data.frame")) {
      new_map <- dplyr::bind_rows(.map, new_map)
    }
  }

  class(new_map) <- c("try_trait_map", class(new_map))
  new_map
}


#' Read a trait map from a CSV file
#'
#' Reads a two-column CSV mapping original TRY trait names to short names.
#'
#' @param file Path to a CSV file with at least two columns.
#' @param from_col Name of the column with original trait names. Defaults to `"original"`.
#' @param to_col Name of the column with short names. Defaults to `"short_name"`.
#'
#' @return A `try_trait_map` object (tibble with `original` and `short_name` columns).
#' @export
read_trait_map <- function(file, from_col = "original", to_col = "short_name") {
  if (!file.exists(file)) {
    cli::cli_abort("File not found: {.path {file}}")
  }

  raw <- utils::read.csv(file, stringsAsFactors = FALSE)

  if (!from_col %in% names(raw) || !to_col %in% names(raw)) {
    cli::cli_abort(
      "CSV must contain columns {.val {from_col}} and {.val {to_col}}. Found: {.val {names(raw)}}"
    )
  }

  out <- tibble::tibble(
    original = raw[[from_col]],
    short_name = raw[[to_col]]
  )

  class(out) <- c("try_trait_map", class(out))
  out
}


#' Load the default trait name mapping
#'
#' Returns the trait name mapping shipped with tidyTRY. This includes
#' common mappings such as SLA, LDMC, leaf nitrogen, seed mass, etc.
#'
#' @return A `try_trait_map` object.
#' @export
default_trait_map <- function() {
  f <- system.file("extdata", "default_trait_map.csv", package = "tidyTRY")
  if (f == "") {
    cli::cli_abort("Default trait map file not found. Package may not be installed correctly.")
  }
  read_trait_map(f)
}


#' Rename traits in TRY data
#'
#' Applies a trait name mapping to a TRY dataset, replacing long TRY trait
#' names with short codes. Unmapped traits are kept as-is.
#'
#' @param data A data frame of TRY data.
#' @param map A trait map object (from [trait_map()], [read_trait_map()], or
#'   [default_trait_map()]). Can also be a named character vector where
#'   names are original trait names and values are short names.
#' @param trait_col Name of the trait name column. Defaults to `"TraitName"`.
#' @param warn_unmapped Logical. Warn about trait names not in the map?
#'   Defaults to `TRUE`.
#'
#' @return The data frame with renamed trait values.
#' @export
rename_traits <- function(data, map, trait_col = "TraitName", warn_unmapped = TRUE) {
  # Normalize the map to a named vector: names = original, values = short_name
  if (inherits(map, "data.frame")) {
    lookup <- stats::setNames(map$short_name, map$original)
  } else if (is.character(map) && !is.null(names(map))) {
    lookup <- map
  } else {
    cli::cli_abort(
      "{.arg map} must be a trait map data frame or a named character vector."
    )
  }

  original_names <- unique(data[[trait_col]])
  original_names <- original_names[!is.na(original_names)]
  unmapped <- setdiff(original_names, names(lookup))

  if (warn_unmapped && length(unmapped) > 0) {
    cli::cli_warn(c(
      "{length(unmapped)} trait name{?s} not in the map (kept as-is):",
      set_names(unmapped, rep("*", length(unmapped)))
    ))
  }

  data[[trait_col]] <- ifelse(
    data[[trait_col]] %in% names(lookup),
    lookup[data[[trait_col]]],
    data[[trait_col]]
  )

  n_renamed <- length(intersect(original_names, names(lookup)))
  cli::cli_inform("Renamed {n_renamed} trait name{?s}.")

  data
}
