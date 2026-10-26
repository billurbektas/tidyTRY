#' List TRY data files
#'
#' Lists `.txt` files in a directory, showing their sizes for awareness
#' when working with large TRY exports.
#'
#' @param path Path to a directory containing TRY data files.
#' @param pattern Regex pattern to match filenames. Defaults to `"\\.txt$"`.
#'
#' @return A data frame with columns `file` (full path) and `size` (human-readable).
#' @export
list_try_files <- function(path, pattern = "\\.txt$") {
  if (!dir.exists(path)) {
    cli::cli_abort("Directory does not exist: {.path {path}}")
  }

  files <- list.files(path, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    cli::cli_warn("No files matching pattern {.val {pattern}} found in {.path {path}}")
    return(tibble::tibble(file = character(), size = character()))
  }

  sizes <- file.info(files)$size
  out <- tibble::tibble(
    file = files,
    size = vapply(sizes, .format_size, character(1))
  )

  cli::cli_inform("Found {nrow(out)} TRY data file{?s}:")
  for (i in seq_len(nrow(out))) {
    cli::cli_inform("
 {basename(out$file[i])} ({out$size[i]})")
  }

  out
}


#' Read TRY data files
#'
#' Reads one or more TRY database export files (tab-delimited `.txt`),
#' optionally filtering by a species list. Uses chunked reading to handle
#' very large files (8+ GB) without loading everything into memory.
#'
#' @param files Character vector of file paths, or a single directory path.
#'   If a directory, all `.txt` files in it are used.
#' @param species Character vector of species names to filter on. Matching is
#'   done against both `SpeciesName` and `AccSpeciesName` columns, exactly
#'   (and case-insensitively for `AccSpeciesName`).
#' @param species_ids Integer vector of TRY `AccSpeciesID`s to filter on, as
#'   selected from [try_species_names()] after resolving the TRY names. This
#'   is the safer route: a resolved accepted name is never used as a filter,
#'   so a wrong name cannot pull in another species' records. Rows matching
#'   either `species` or `species_ids` are kept. If both are `NULL`, all rows
#'   are returned (a warning is emitted for large files).
#' @param chunk_size Number of rows to read per chunk. Defaults to 100,000.
#' @param progress Show a progress bar. Defaults to `TRUE`.
#'
#' @return A tibble containing the filtered TRY data.
#' @export
#'
#' @examples
#' \dontrun{
#' # Read from a directory, filtering by species
#' dat <- read_try("data/try", species = c("Papaver rhoeas", "Centaurea cyanus"))
#'
#' # Read specific files
#' dat <- read_try(c("file1.txt", "file2.txt"), species = my_species)
#' }
read_try <- function(files, species = NULL, species_ids = NULL,
                     chunk_size = 100000L, progress = TRUE) {
  files <- .try_files(files)

  if (is.null(species) && is.null(species_ids)) {
    total_size <- sum(file.info(files)$size)
    if (total_size > 1e9) { # > 1 GB
      cli::cli_warn(c(
        "Reading {length(files)} file{?s} ({(.format_size(total_size))}) without species filter.",
        "i" = "This may use a lot of memory. Consider passing {.arg species} or {.arg species_ids}."
      ))
    }
  }

  results <- purrr::map(files, function(f) {
    cli::cli_inform("Reading {.path {basename(f)}}...")
    .read_try_single(f, species = species, species_ids = species_ids,
                     chunk_size = chunk_size, progress = progress)
  })

  out <- dplyr::bind_rows(results)

  # Drop phantom columns from trailing tabs (e.g., X29, ...30)
  phantom_cols <- grep("^(\\.{3}\\d+|X\\d+)$", names(out), value = TRUE)
  if (length(phantom_cols) > 0) {
    out <- out[, !names(out) %in% phantom_cols, drop = FALSE]
  }

  cli::cli_inform("Read {nrow(out)} rows from {length(files)} file{?s}.")
  out
}


# Internal: read a single TRY file with chunked species filtering
.read_try_single <- function(file, species, species_ids = NULL, chunk_size, progress) {
  # Pre-compute lowercase species set for case-insensitive AccSpeciesName
  # matching (TRY stores some AccSpeciesName in ALL CAPS)
  species_lower <- if (!is.null(species)) tolower(species) else NULL

  callback_fn <- function(chunk, pos) {
    if (is.null(species) && is.null(species_ids)) return(chunk)
    mask <- rep(FALSE, nrow(chunk))
    if (!is.null(species)) {
      mask <- mask | (chunk$SpeciesName %in% species) |
        (chunk$AccSpeciesName %in% species) |
        (tolower(chunk$AccSpeciesName) %in% species_lower)
    }
    if (!is.null(species_ids)) {
      mask <- mask | (chunk$AccSpeciesID %in% species_ids)
    }
    chunk[mask, , drop = FALSE]
  }

  readr::read_tsv_chunked(
    file,
    callback = readr::DataFrameCallback$new(callback_fn),
    col_names = .try_header(file),
    skip = 1L,
    chunk_size = chunk_size,
    col_types = .try_col_spec,
    progress = progress,
    show_col_types = FALSE
  )
}


# Internal: expand a directory into its .txt files and check they exist
.try_files <- function(files) {
  if (length(files) == 1 && dir.exists(files)) {
    dir_path <- files
    files <- list.files(dir_path, pattern = "\\.txt$", full.names = TRUE)
    if (length(files) == 0) {
      cli::cli_abort("No .txt files found in {.path {dir_path}}")
    }
  }
  missing <- files[!file.exists(files)]
  if (length(missing) > 0) {
    cli::cli_abort("File{?s} not found: {.path {missing}}")
  }
  files
}


# Internal: column names of a TRY file. TRY exports often have a trailing tab,
# creating unnamed phantom columns; they get explicit names so readr never
# needs to auto-fill names (which triggers a warning).
.try_header <- function(file) {
  header_line <- readr::read_lines(file, n_max = 1)
  col_names <- strsplit(header_line, "\t")[[1]]
  empty <- col_names == ""
  if (any(empty)) {
    col_names[empty] <- paste0(".phantom_", seq_len(sum(empty)))
  }
  col_names
}
