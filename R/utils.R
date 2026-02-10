#' @importFrom readr cols col_character col_double col_integer
#' @importFrom rlang .data check_installed
NULL

# Column type specification for TRY data files.
# This ensures consistent parsing across all TRY exports and avoids
# the type-coercion issues that occur with automatic detection.
.try_col_spec <- readr::cols(
  LastName = readr::col_character(),
  FirstName = readr::col_character(),
  DatasetID = readr::col_integer(),
  Dataset = readr::col_character(),
  SpeciesName = readr::col_character(),
  AccSpeciesID = readr::col_integer(),
  AccSpeciesName = readr::col_character(),
  ObservationID = readr::col_integer(),
  ObsDataID = readr::col_integer(),
  TraitID = readr::col_integer(),
  TraitName = readr::col_character(),
  DataID = readr::col_integer(),
  DataName = readr::col_character(),
  OriglName = readr::col_character(),
  OrigValueStr = readr::col_character(),
  OrigUnitStr = readr::col_character(),
  ValueKindName = readr::col_character(),
  OrigUncertaintyStr = readr::col_character(),
  UncertaintyName = readr::col_character(),
  Replicates = readr::col_character(),
  StdValue = readr::col_double(),
  UnitName = readr::col_character(),
  RelUncertaintyPercent = readr::col_double(),
  OrigObsDataID = readr::col_integer(),
  ErrorRisk = readr::col_double(),
  Reference = readr::col_character(),
  Comment = readr::col_character(),
  StdValueStr = readr::col_character(),
  # TRY exports often have a trailing tab, creating a phantom 29th column.
  # .default = col_skip() silently discards any unnamed/extra columns.
  .default = readr::col_skip()
)

# Format file size in human-readable form
.format_size <- function(bytes) {
  units <- c("B", "KB", "MB", "GB", "TB")
  idx <- 1
  size <- bytes
  while (size >= 1024 && idx < length(units)) {
    size <- size / 1024
    idx <- idx + 1
  }
  sprintf("%.1f %s", size, units[idx])
}
