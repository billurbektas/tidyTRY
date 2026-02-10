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
#'   done against both `SpeciesName` and `AccSpeciesName` columns. If `NULL`,
#'   all rows are returned (a warning is emitted for large files).
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
read_try <- function(files, species = NULL, chunk_size = 100000L, progress = TRUE) {
  # If a directory is passed, find all .txt files in it

if (length(files) == 1 && dir.exists(files)) {
    dir_path <- files
    files <- list.files(dir_path, pattern = "\\.txt$", full.names = TRUE)
    if (length(files) == 0) {
      cli::cli_abort("No .txt files found in {.path {dir_path}}")
    }
  }

  # Validate files exist
  missing <- files[!file.exists(files)]
  if (length(missing) > 0) {
    cli::cli_abort("File{?s} not found: {.path {missing}}")
  }

  if (is.null(species)) {
    total_size <- sum(file.info(files)$size)
    if (total_size > 1e9) { # > 1 GB
      cli::cli_warn(c(
        "Reading {length(files)} file{?s} ({.format_size(total_size)}) without species filter.",
        "i" = "This may use a lot of memory. Consider passing a {.arg species} vector."
      ))
    }
  }

  results <- purrr::map(files, function(f) {
    cli::cli_inform("Reading {.path {basename(f)}}...")
    .read_try_single(f, species = species, chunk_size = chunk_size, progress = progress)
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
.read_try_single <- function(file, species, chunk_size, progress) {
  callback_fn <- function(chunk, pos) {
    if (is.null(species)) return(chunk)
    mask <- (chunk$SpeciesName %in% species) | (chunk$AccSpeciesName %in% species)
    chunk[mask, , drop = FALSE]
  }

  readr::read_tsv_chunked(
    file,
    callback = readr::DataFrameCallback$new(callback_fn),
    chunk_size = chunk_size,
    col_types = .try_col_spec,
    progress = progress,
    show_col_types = FALSE
  )
}
