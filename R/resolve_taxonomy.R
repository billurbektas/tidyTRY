#' Resolve species names
#'
#' Harmonizes species names using a taxonomic backbone. Supports two backends:
#' TNRS (online; WCVP first, WFO as fallback) and WorldFlora (offline after a
#' one-time backbone download).
#'
#' @param species Character vector of species names to resolve. Author
#'   strings may be included ("Agrostis alpina Scop."); TNRS uses them to tell
#'   homonyms apart.
#' @param method Taxonomy resolution method. One of `"tnrs"` (default,
#'   requires internet) or `"worldflora"` (offline, requires backbone file).
#' @param wfo_backbone Path to the World Flora Online backbone file. Only used
#'   when `method = "worldflora"`. If `NULL`, checks
#'   `getOption("tidyTRY.wfo_backbone")`.
#' @param fuzzy Logical. Enable fuzzy matching? Defaults to `TRUE`. Only
#'   relevant for the WorldFlora backend.
#' @param ... Additional arguments passed to the backend. For TNRS:
#'   `sources` (default `c("wcvp", "wfo")`, in order of priority),
#'   `batch_size` (default 5000) and `retries` (default 2: names returned
#'   without any candidate are submitted again up to this many times), plus
#'   any argument of [TNRS::TNRS()] except `matches`, which is always
#'   `"all"`.
#'
#' @return A tibble with one row per unique submitted name:
#'   \describe{
#'     \item{submitted_name}{The name submitted for resolution.}
#'     \item{accepted_name}{The accepted name, or `NA` when no single
#'       legitimate name could be chosen (see `needs_review`).}
#'     \item{matched_name}{The name matched in the backbone.}
#'     \item{source}{The backbone the accepted name was taken from.}
#'     \item{score}{Match quality score (interpretation depends on backend).}
#'     \item{taxonomic_status}{Status of the matched name: `"Accepted"` or
#'       `"Synonym"`.}
#'     \item{accepted_author, accepted_species, accepted_family}{Author,
#'       species-rank name and family of the accepted name.}
#'     \item{n_legitimate}{Number of distinct accepted names among the
#'       legitimate candidates.}
#'     \item{tnrs_warning}{TNRS warnings, e.g. `"[Ambiguous]"` for homonyms.}
#'     \item{needs_review}{`TRUE` when the name must be checked by hand: no
#'       legitimate candidate, a tie between accepted names, or a name taken
#'       from a fallback source.}
#'     \item{review_reason}{Why `needs_review` is `TRUE`.}
#'   }
#'
#' @details
#' ## TNRS backend
#' Uses the [TNRS](https://tnrs.biendata.org/) API via the `TNRS` R package and
#' requires an internet connection. All candidate matches are requested
#' (`matches = "all"`), and one is chosen as follows:
#' 1. only legitimate candidates are considered: taxonomic status
#'    `"Accepted"` or `"Synonym"` with a non-empty accepted name;
#'    `"Illegitimate"`, `"Invalid"` and `"No opinion"` records are never used;
#' 2. the highest overall score;
#' 3. the first source in `sources` (WCVP before WFO by default);
#' 4. `"Accepted"` before `"Synonym"`.
#'
#' A fuzzy match into another genus (e.g. "Carex lehmanii" matched to
#' *Carum lemmonii*) is never chosen. If more than one accepted name is still
#' tied, or no legitimate candidate exists, `accepted_name` is `NA` and
#' `needs_review` is `TRUE`. A name from the fallback source, or matched with
#' a score below 0.9, is returned and flagged. Candidate rows that TNRS
#' returns for an unrelated name (another genus with a perfect name score)
#' are dropped with a warning. Before
#' version 0.2.0 the TNRS "best" match was used as returned. For names with
#' homonyms that match can be an illegitimate name: "Festuca pratensis"
#' resolved to *Festuca ovina* instead of *Lolium pratense*.
#'
#' An error is raised if TNRS returns nothing (e.g. offline), instead of
#' returning an empty result.
#'
#' ## WorldFlora backend
#' Uses the `WorldFlora` R package with a locally stored WFO backbone file.
#' Download the backbone once from <https://www.worldfloraonline.org/downloadData>
#' (the "classification" file). After downloading, all matching is fully offline.
#' The single best match is chosen by [WorldFlora::WFO.one()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Using TNRS (online)
#' resolved <- resolve_species(c("Papaver rhoeas", "Festuca pratensis"))
#' resolved[resolved$needs_review, ]
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

  n_resolved <- sum(!is.na(result$accepted_name))
  n_review <- sum(result$needs_review)
  cli::cli_inform(
    "Resolved {n_resolved}/{length(species)} name{?s}; {n_review} need{?s/} review."
  )

  result
}


# Internal: TNRS backend
#
# TNRS's own "best" match is the candidate ranked first by Overall_score. For
# names with homonyms every candidate scores 1, and an Illegitimate or Invalid
# homonym can be ranked first (e.g. "Festuca pratensis" -> Festuca ovina, an
# illegitimate later homonym, instead of Lolium pratense). So all candidates
# are requested and the choice is made here, from legitimate names only.
.resolve_tnrs <- function(species, sources = c("wcvp", "wfo"), batch_size = 5000L,
                          retries = 2L, ...) {
  rlang::check_installed("TNRS", reason = "to use the TNRS taxonomy backend")
  out <- .select_tnrs(.tnrs_candidates(species, sources, batch_size, ...), species, sources = sources)
  # Names whose candidate rows were lost to faulty responses come back with
  # no candidates at all; most return normally when submitted again
  for (k in seq_len(retries)) {
    miss <- which(out$review_reason %in% "no match")
    if (!length(miss)) break
    cli::cli_inform("Re-submitting {length(miss)} name{?s} returned without candidates (attempt {k} of {retries}).")
    again <- .tnrs_candidates(species[miss], sources, batch_size, ...)
    out[miss, ] <- .select_tnrs(again, species[miss], sources = sources)
  }
  out
}


# Internal: all TNRS candidates for `species`, in batches, with IDs indexing
# `species`
.tnrs_candidates <- function(species, sources, batch_size, ...) {
  batches <- split(seq_along(species), ceiling(seq_along(species) / batch_size))
  res <- purrr::map(batches, function(idx) {
    out <- TNRS::TNRS(species[idx], sources = sources, matches = "all", ...)
    if (is.null(out) || !nrow(out)) {
      cli::cli_abort(c(
        "TNRS returned no result.",
        "i" = "Check the internet connection and the TNRS service; nothing was resolved."
      ))
    }
    # TNRS numbers the submitted names 1..n within a call
    out$ID <- idx[as.integer(out$ID)]
    out
  })
  dplyr::bind_rows(res)
}


# Internal: choose one accepted name per submitted name from TNRS candidates
# (TNRS::TNRS(..., matches = "all") output).
#
# Rules, in order:
#   1. only legitimate candidates: Taxonomic_status "Accepted" or "Synonym",
#      with a non-empty Accepted_name;
#   2. highest Overall_score;
#   3. the earliest source in `sources` (Source can read "wcvp,wfo");
#   4. "Accepted" before "Synonym".
# If more than one accepted name is still tied, or no legitimate candidate
# exists, accepted_name is NA and needs_review is TRUE. A match into another
# genus is never chosen. A name taken from a source other than the first in
# `sources`, or matched with a score below `min_score`, is returned but
# flagged.
.select_tnrs <- function(res, species, sources = c("wcvp", "wfo"), min_score = 0.9) {
  needed <- c("ID", "Name_submitted", "Name_matched", "Taxonomic_status",
              "Accepted_name", "Overall_score", "Source")
  missing_cols <- setdiff(needed, names(res))
  if (length(missing_cols)) {
    cli::cli_abort("TNRS output lacks column{?s} {.val {missing_cols}}.")
  }

  # A candidate belongs to the name it was submitted for. The TNRS service has
  # been seen to return a candidate of one name under the ID of another
  # (Hypochaeris maculata under the ID of Euphorbia pekinensis, Oct 2026), so
  # the ID is checked against Name_submitted and corrected by name; a row
  # whose submitted name matches no input name is dropped.
  # TNRS rewrites the submitted string: commas become spaces and accented
  # letters plain ones ("Torr. & Frém." -> "Torr. & Frem."), so names are
  # compared on their letters and digits only
  norm <- function(x) {
    x <- as.character(x)
    ascii <- iconv(x, to = "ASCII//TRANSLIT", sub = "")
    ascii[is.na(ascii)] <- x[is.na(ascii)]
    tolower(gsub("[^[:alnum:]]", "", ascii))
  }
  id <- as.integer(res$ID)
  by_name <- match(norm(res$Name_submitted), norm(species))
  wrong <- is.na(id) | id < 1L | id > length(species) | norm(species[id]) != norm(res$Name_submitted)
  wrong[is.na(wrong)] <- TRUE
  if (any(wrong)) {
    n_fixed <- sum(wrong & !is.na(by_name))
    n_dropped <- sum(wrong & is.na(by_name))
    cli::cli_warn(c(
      "TNRS returned {sum(wrong)} candidate row{?s} under the ID of another name.",
      "i" = "{n_fixed} reassigned by submitted name, {n_dropped} dropped."
    ))
    id[wrong] <- by_name[wrong]
    res <- res[!is.na(id), , drop = FALSE]
    id <- id[!is.na(id)]
  }
  res$ID <- id

  # The service has also been seen to return another name's candidate under
  # the right ID and submitted name: Name_matched is then an unrelated name
  # in another genus, yet Name_score is 1 (Bromus tectorum -> Veronica
  # serpyllifolia). A true match cannot be both, so such rows are dropped.
  genus <- function(x) tolower(sub("\\s.*$", "", trimws(as.character(x))))
  other_genus <- !is.na(res$Name_matched) & nzchar(res$Name_matched) &
    genus(res$Name_matched) != genus(res$Name_submitted)
  name_score <- if ("Name_score" %in% names(res)) suppressWarnings(as.numeric(res$Name_score)) else rep(NA_real_, nrow(res))
  corrupted <- other_genus & !is.na(name_score) & name_score >= 1
  if (any(corrupted)) {
    cli::cli_warn(c(
      "TNRS returned {sum(corrupted)} candidate row{?s} of an unrelated name (another genus with a perfect name score); dropped.",
      "i" = "Affected: {.val {unique(res$Name_submitted[corrupted])}}."
    ))
    res <- res[!corrupted, , drop = FALSE]
    other_genus <- other_genus[!corrupted]
  }
  opt <- function(col) if (col %in% names(res)) as.character(res[[col]]) else rep(NA_character_, nrow(res))

  cand <- tibble::tibble(
    id = as.integer(res$ID),
    matched_name = as.character(res$Name_matched),
    taxonomic_status = as.character(res$Taxonomic_status),
    accepted_name = dplyr::na_if(trimws(as.character(res$Accepted_name)), ""),
    accepted_author = dplyr::na_if(trimws(opt("Accepted_name_author")), ""),
    accepted_species = dplyr::na_if(trimws(opt("Accepted_species")), ""),
    accepted_family = dplyr::na_if(trimws(opt("Accepted_family")), ""),
    source = as.character(res$Source),
    score = suppressWarnings(as.numeric(res$Overall_score)),
    tnrs_warning = dplyr::na_if(trimws(opt("WarningsEng")), ""),
    other_genus = other_genus
  )
  cand$source_rank <- vapply(strsplit(cand$source, ",", fixed = TRUE), function(s) {
    r <- match(trimws(s), sources)
    if (all(is.na(r))) NA_integer_ else min(r, na.rm = TRUE)
  }, integer(1))
  cand$status_rank <- match(cand$taxonomic_status, c("Accepted", "Synonym"))
  cand$legitimate <- !is.na(cand$status_rank) & !is.na(cand$accepted_name) &
    !is.na(cand$source_rank)
  # a fuzzy match into another genus is a different plant until shown
  # otherwise (Carex lehmanii -> Carum lemmonii); it is never chosen
  # automatically
  cand$automatic <- cand$legitimate & !cand$other_genus

  pick_one <- function(id) {
    all_c <- cand[cand$id == id, , drop = FALSE]
    legit <- all_c[all_c$automatic, , drop = FALSE]
    elsewhere <- all_c[all_c$legitimate & all_c$other_genus, , drop = FALSE]
    base <- tibble::tibble(
      submitted_name = species[id], accepted_name = NA_character_,
      matched_name = if (nrow(all_c)) all_c$matched_name[1] else NA_character_,
      source = NA_character_, score = NA_real_, taxonomic_status = NA_character_,
      accepted_author = NA_character_, accepted_species = NA_character_,
      accepted_family = NA_character_,
      n_legitimate = length(unique(legit$accepted_name)),
      tnrs_warning = paste(unique(stats::na.omit(all_c$tnrs_warning)), collapse = " "),
      needs_review = TRUE, review_reason = NA_character_
    )
    if (!nrow(legit)) {
      base$review_reason <- if (nrow(elsewhere)) {
        paste("only matches in another genus:", paste(sort(unique(elsewhere$matched_name)), collapse = " | "))
      } else if (nrow(all_c)) "no legitimate candidate" else "no match"
      return(base)
    }
    top <- legit[legit$score == max(legit$score, na.rm = TRUE), , drop = FALSE]
    top <- top[top$source_rank == min(top$source_rank), , drop = FALSE]
    top <- top[top$status_rank == min(top$status_rank), , drop = FALSE]
    names_left <- unique(top$accepted_name)
    if (length(names_left) > 1) {
      base$review_reason <- paste("tie:", paste(sort(names_left), collapse = " | "))
      return(base)
    }
    chosen <- top[1, ]
    base$accepted_name <- chosen$accepted_name
    base$matched_name <- chosen$matched_name
    base$source <- sources[chosen$source_rank]
    base$score <- chosen$score
    base$taxonomic_status <- chosen$taxonomic_status
    base$accepted_author <- chosen$accepted_author
    base$accepted_species <- chosen$accepted_species
    base$accepted_family <- chosen$accepted_family
    reasons <- c(if (chosen$source_rank > 1L) paste("fallback source", base$source),
                 if (!is.na(chosen$score) && chosen$score < min_score) sprintf("fuzzy match (score %.2f)", chosen$score))
    base$needs_review <- length(reasons) > 0
    base$review_reason <- if (length(reasons)) paste(reasons, collapse = "; ") else NA_character_
    base
  }

  out <- dplyr::bind_rows(lapply(seq_along(species), pick_one))
  out$tnrs_warning <- dplyr::na_if(out$tnrs_warning, "")
  out
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

  # WFO.match can return multiple rows per species (e.g., when a name
  # is a synonym of multiple accepted names). WFO.one selects the best
  # single match per species, prioritizing "Accepted" status.
  res <- WorldFlora::WFO.one(res, verbose = FALSE)

  accepted <- dplyr::na_if(trimws(res$scientificName), "")
  tibble::tibble(
    submitted_name = res$spec.name,
    accepted_name = accepted,
    matched_name = ifelse(
      res$New.accepted & res$Old.name != "",
      res$Old.name,
      res$scientificName
    ),
    source = "WFO",
    score = ifelse(res$Fuzzy, res$Fuzzy.dist, 0),
    taxonomic_status = if ("taxonomicStatus" %in% names(res)) res$taxonomicStatus else NA_character_,
    accepted_author = if ("scientificNameAuthorship" %in% names(res)) res$scientificNameAuthorship else NA_character_,
    accepted_species = NA_character_,
    accepted_family = if ("family" %in% names(res)) res$family else NA_character_,
    n_legitimate = NA_integer_,
    tnrs_warning = NA_character_,
    needs_review = is.na(accepted),
    review_reason = ifelse(is.na(accepted), "no match", NA_character_)
  )
}
