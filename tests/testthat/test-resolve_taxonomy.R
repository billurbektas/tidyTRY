resolve_columns <- c(
  "submitted_name", "accepted_name", "matched_name", "source", "score",
  "taxonomic_status", "accepted_author", "accepted_species", "accepted_family",
  "n_legitimate", "tnrs_warning", "needs_review", "review_reason"
)

# Saved TNRS::TNRS(..., sources = c("wcvp", "wfo"), matches = "all") output
fixture_selection <- function() {
  r <- utils::read.csv(test_path("fixtures", "tnrs_all_matches.csv"), stringsAsFactors = FALSE)
  species <- unique(r$Name_submitted)
  r$ID <- match(r$Name_submitted, species)
  tidyTRY:::.select_tnrs(r, species)
}

test_that("resolve_species validates input", {
  expect_error(resolve_species(c(NA, "", "  ")), "No valid species")
})

test_that("resolve_species removes NAs and blanks before resolving", {
  skip_if_not_installed("TNRS")
  skip_if_offline()

  result <- resolve_species(c("Papaver rhoeas", NA, ""), method = "tnrs")
  expect_equal(nrow(result), 1)
  expect_equal(result$submitted_name, "Papaver rhoeas")
})

test_that("resolve_species works with TNRS", {
  skip_if_not_installed("TNRS")
  skip_if_offline()

  result <- resolve_species(
    c("Papaver rhoeas", "Centaurea cyanus"),
    method = "tnrs"
  )

  expect_s3_class(result, "tbl_df")
  expect_named(result, resolve_columns)
  expect_equal(nrow(result), 2)
})

test_that("resolve_species errors with worldflora and no backbone", {
  skip_if_not_installed("WorldFlora")

  expect_error(
    resolve_species("Papaver rhoeas", method = "worldflora"),
    "backbone"
  )
})


test_that("an illegitimate homonym ranked first by TNRS is never chosen", {
  sel <- fixture_selection()
  pick <- stats::setNames(sel$accepted_name, sel$submitted_name)
  # TNRS "best" returns the Illegitimate homonyms Festuca ovina and
  # Agrostis rupestris subsp. rupestris for these two names
  expect_equal(pick[["Festuca pratensis"]], "Lolium pratense")
  expect_equal(pick[["Agrostis alpina"]], "Alpagrostis alpina")
  expect_equal(pick[["Papaver rhoeas"]], "Papaver rhoeas")
  expect_true(all(sel$taxonomic_status[!is.na(sel$accepted_name)] %in% c("Accepted", "Synonym")))
})

test_that("an accepted name beats a synonym of another author's homonym", {
  sel <- fixture_selection()
  expect_equal(sel$accepted_name[sel$submitted_name == "Bouteloua gracilis"], "Bouteloua gracilis")
})

test_that("ties and names without a legitimate candidate are left for review", {
  sel <- fixture_selection()
  tie <- sel[sel$submitted_name == "Senecio alpinus", ]
  expect_true(is.na(tie$accepted_name))
  expect_true(tie$needs_review)
  expect_match(tie$review_reason, "^tie: Jacobaea alpina \\| Senecio albopurpureus$")
  # Koeleria cristata: every WCVP candidate is illegitimate; three WFO names tie
  kc <- sel[sel$submitted_name == "Koeleria cristata", ]
  expect_true(is.na(kc$accepted_name))
  expect_true(kc$needs_review)
})

test_that("empty accepted names count as missing", {
  sel <- fixture_selection()
  expect_false(any(sel$accepted_name %in% ""))
  expect_named(sel, resolve_columns)
})

test_that("a name from the fallback source is returned but flagged", {
  r <- data.frame(
    ID = 1L, Name_submitted = "Xus yus", Name_matched = "Xus yus",
    Taxonomic_status = "Synonym", Accepted_name = "Xus zus",
    Overall_score = 1, Source = "wfo", stringsAsFactors = FALSE
  )
  sel <- tidyTRY:::.select_tnrs(r, "Xus yus")
  expect_equal(sel$accepted_name, "Xus zus")
  expect_true(sel$needs_review)
  expect_equal(sel$review_reason, "fallback source wfo")
})

test_that("resolve_species errors when TNRS returns nothing", {
  skip_if_not_installed("TNRS")
  local_mocked_bindings(TNRS = function(...) NULL, .package = "TNRS")
  expect_error(resolve_species("Papaver rhoeas"), "TNRS returned no result")
})

test_that("a candidate returned under another name's ID is reassigned by its submitted name", {
  r <- utils::read.csv(test_path("fixtures", "tnrs_all_matches.csv"), stringsAsFactors = FALSE)
  species <- unique(r$Name_submitted)
  r$ID <- match(r$Name_submitted, species)
  # move one Agrostis alpina candidate under the ID of Papaver rhoeas
  i <- which(r$Name_submitted == "Agrostis alpina" & r$Accepted_name == "Alpagrostis alpina")[1]
  r$ID[i] <- match("Papaver rhoeas", species)
  expect_warning(sel <- tidyTRY:::.select_tnrs(r, species), "under the ID of another name")
  pick <- stats::setNames(sel$accepted_name, sel$submitted_name)
  expect_equal(pick[["Papaver rhoeas"]], "Papaver rhoeas")
  expect_equal(pick[["Agrostis alpina"]], "Alpagrostis alpina")
})

test_that("a candidate whose submitted name matches no input is dropped", {
  r <- data.frame(
    ID = c(1L, 1L), Name_submitted = c("Papaver rhoeas", "Something else"),
    Name_matched = c("Papaver rhoeas", "Something else"),
    Taxonomic_status = c("Accepted", "Accepted"),
    Accepted_name = c("Papaver rhoeas", "Something else"),
    Overall_score = c(1, 1), Source = c("wcvp", "wcvp"), stringsAsFactors = FALSE
  )
  expect_warning(sel <- tidyTRY:::.select_tnrs(r, "Papaver rhoeas"), "1 dropped")
  expect_equal(sel$accepted_name, "Papaver rhoeas")
  expect_false(sel$needs_review)
})

tnrs_rows <- function(...) {
  d <- data.frame(..., stringsAsFactors = FALSE)
  d$Source <- if (is.null(d$Source)) "wcvp" else d$Source
  d
}

test_that("a candidate of an unrelated name with a perfect name score is dropped", {
  r <- tnrs_rows(
    ID = c(1L, 1L), Name_submitted = "Bromus tectorum",
    Name_matched = c("Bromus tectorum", "Veronica serpyllifolia"),
    Name_score = c(1, 1), Overall_score = c(1, 0.9),
    Taxonomic_status = "Accepted", Accepted_name = c("Bromus tectorum", "Veronica serpyllifolia")
  )
  expect_warning(sel <- tidyTRY:::.select_tnrs(r, "Bromus tectorum"), "unrelated name")
  expect_equal(sel$accepted_name, "Bromus tectorum")
  expect_equal(sel$n_legitimate, 1L)
})

test_that("a fuzzy match into another genus is never chosen", {
  r <- tnrs_rows(
    ID = 1L, Name_submitted = "Carex lehmanii", Name_matched = "Carum lemmonii",
    Name_score = 0.61, Overall_score = 0.61,
    Taxonomic_status = "Synonym", Accepted_name = "Perideridia lemmonii"
  )
  sel <- tidyTRY:::.select_tnrs(r, "Carex lehmanii")
  expect_true(is.na(sel$accepted_name))
  expect_true(sel$needs_review)
  expect_match(sel$review_reason, "only matches in another genus: Carum lemmonii")
})

test_that("a fuzzy match below min_score is returned but flagged", {
  r <- tnrs_rows(
    ID = 1L, Name_submitted = "Carex lehmanii", Name_matched = "Carex lehmannii",
    Name_score = 0.85, Overall_score = 0.85,
    Taxonomic_status = "Accepted", Accepted_name = "Carex lehmannii"
  )
  sel <- tidyTRY:::.select_tnrs(r, "Carex lehmanii")
  expect_equal(sel$accepted_name, "Carex lehmannii")
  expect_true(sel$needs_review)
  expect_match(sel$review_reason, "fuzzy match \\(score 0.85\\)")
})

test_that("names returned without candidates are submitted again", {
  skip_if_not_installed("TNRS")
  calls <- 0L
  fake <- function(taxonomic_names, ...) {
    calls <<- calls + 1L
    keep <- if (calls == 1L) taxonomic_names[taxonomic_names != "Festuca pratensis"] else taxonomic_names
    ids <- match(keep, taxonomic_names)
    data.frame(ID = ids, Name_submitted = keep, Name_matched = keep, Name_score = 1,
               Overall_score = 1, Taxonomic_status = "Accepted", Accepted_name = keep,
               Source = "wcvp", stringsAsFactors = FALSE)
  }
  local_mocked_bindings(TNRS = fake, .package = "TNRS")
  res <- resolve_species(c("Papaver rhoeas", "Festuca pratensis"))
  expect_equal(calls, 2L)
  expect_equal(res$accepted_name, c("Papaver rhoeas", "Festuca pratensis"))
})

test_that("TNRS's rewriting of commas and accents does not separate a candidate from its name", {
  sent <- c("Atriplex confertifolia (Torr. & Frém.) S. Watson", "Agrostis hyemalis (Walter) Britton, Sterns & Poggenb.")
  r <- data.frame(
    ID = 1:2, Name_submitted = c("Atriplex confertifolia (Torr. & Frem.) S. Watson",
                                 "Agrostis hyemalis (Walter) Britton  Sterns & Poggenb."),
    Name_matched = c("Atriplex confertifolia", "Agrostis hyemalis"), Name_score = 1, Overall_score = 1,
    Taxonomic_status = "Accepted", Accepted_name = c("Atriplex confertifolia", "Agrostis hyemalis"),
    Source = "wcvp", stringsAsFactors = FALSE
  )
  expect_no_warning(sel <- tidyTRY:::.select_tnrs(r, sent))
  expect_equal(sel$accepted_name, c("Atriplex confertifolia", "Agrostis hyemalis"))
})
