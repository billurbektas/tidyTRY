#' Resolve species names
#'
#' Harmonizes species names using a taxonomic backbone. Supports two backends:
#' TNRS (online, uses WCVP + WFO) and WorldFlora (offline after one-time
#' backbone download).
#'
#' @param species Character vector of species names to resolve.
#' @param method Taxonomy resolution method. One of `"tnrs"` (default,
#'   requires internet) or `"worldflora"` (offline, requires backbone file).
#' @param wfo_backbone Path to the World Flora Online backbone file. Only used
#'   when `method = "worldflora"`. If `NULL`, checks
#'   `getOption("tidyTRY.wfo_backbone")`.
#' @param fuzzy Logical. Enable fuzzy matching? Defaults to `TRUE`. Only
#'   relevant for the WorldFlora backend.
#' @param ... Additional arguments passed to the underlying resolution function.
#'
#' @return A tibble with standardized columns:
#'   \describe{
#'     \item{submitted_name}{The original name submitted for resolution.}
#'     \item{accepted_name}{The accepted/resolved taxonomic name.}
#'     \item{matched_name}{The name that was matched in the database.}
#'     \item{source}{The taxonomic source used for matching.}
#'     \item{score}{Match quality score (interpretation depends on backend).}
#'   }
#'
#' @details
#' ## TNRS backend
#' Uses the [TNRS](https://tnrs.biendata.org/) API via the `TNRS` R package.
#' Requires an internet connection. Handles batches of up to 5,000 names
#' automatically.
#'
#' ## WorldFlora backend
#' Uses the `WorldFlora` R package with a locally stored WFO backbone file.
#' Download the backbone once from <https://www.worldfloraonline.org/downloadData>
#' (the "classification" file). After downloading, all matching is fully offline.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Using TNRS (online)
#' resolved <- resolve_species(c("Papaver rhoeas", "Centaurea cyanus"))
#'
#' # Using WorldFlora (offline)
#' resolved <- resolve_species(
#'   c("Papaver rhoeas", "Centaurea cyanus"),
#'   method = "worldflora",
#'   wfo_backbone = "path/to/classification.csv"
#' )
#' }
resolve_species <- function(species,
                            method = c("tnrs", "worldflora"),
                            wfo_backbone = NULL,
                            fuzzy = TRUE,
                            ...) {
  method <- match.arg(method)

  # Remove NAs and blanks
  species <- unique(species[!is.na(species) & trimws(species) != ""])

  if (length(species) == 0) {
    cli::cli_abort("No valid species names provided.")
  }

  cli::cli_inform("Resolving {length(species)} species name{?s} using {.val {method}}...")

  result <- switch(method,
    tnrs = .resolve_tnrs(species, ...),
    worldflora = .resolve_worldflora(species, wfo_backbone = wfo_backbone, fuzzy = fuzzy, ...)
  )

  n_resolved <- sum(!is.na(result$accepted_name) & result$accepted_name != "")
  cli::cli_inform(
    "Resolved {n_resolved}/{length(species)} name{?s}."
  )

  result
}


# Internal: TNRS backend
.resolve_tnrs <- function(species, ...) {
  rlang::check_installed("TNRS", reason = "to use the TNRS taxonomy backend")

  res <- TNRS::TNRS(species, ...)

  tibble::tibble(
    submitted_name = res$Name_submitted,
    accepted_name = res$Accepted_name,
    matched_name = res$Name_matched,
    source = res$Source,
    score = res$Overall_score
  )
}


# Internal: WorldFlora backend
.resolve_worldflora <- function(species, wfo_backbone = NULL, fuzzy = TRUE, ...) {
  rlang::check_installed("WorldFlora", reason = "to use the WorldFlora taxonomy backend")

  if (is.null(wfo_backbone)) {
    wfo_backbone <- getOption("tidyTRY.wfo_backbone")
  }

  if (is.null(wfo_backbone) || !file.exists(wfo_backbone)) {
    cli::cli_abort(c(
      "WorldFlora backbone file not found.",
      "i" = "Download it from {.url https://www.worldfloraonline.org/downloadData}",
      "i" = "Then pass the path via {.arg wfo_backbone} or set {.code options(tidyTRY.wfo_backbone = 'path/to/file')}"
    ))
  }

  res <- WorldFlora::WFO.match(
    spec.data = data.frame(spec.name = species),
    WFO.file = wfo_backbone,
    Fuzzy = if (fuzzy) 0.1 else 0,
    ...
  )

  tibble::tibble(
    submitted_name = res$spec.name,
    accepted_name = res$scientificName,
    matched_name = ifelse(
      res$New.accepted & res$Old.name != "",
      res$Old.name,
      res$scientificName
    ),
    source = "WFO",
    score = ifelse(res$Fuzzy, res$Fuzzy.dist, 0)
  )
}
