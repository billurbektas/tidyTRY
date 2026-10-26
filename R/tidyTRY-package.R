#' @keywords internal
"_PACKAGE"

# Imports formerly listed by hand in NAMESPACE, kept so roxygen regenerates
# the same NAMESPACE.
#' @importFrom dplyr across bind_rows distinct everything filter group_by
#' @importFrom dplyr if_else left_join mutate n pull rename select summarize
#' @importFrom dplyr ungroup
#' @importFrom readr DataFrameCallback read_tsv_chunked
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom utils read.csv write.csv
#' @importFrom rlang :=
NULL

# Column names used unquoted in dplyr::rename() in read_indicators() and
# read_dispersal()
utils::globalVariables(c("species_source", "species_TNRS"))
