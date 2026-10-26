test_that("get_trait_info extracts unique traits", {
  f <- test_path("fixtures", "sample_try.txt")
  data <- read_try(f)
  info <- get_trait_info(data)

  expect_s3_class(info, "tbl_df")
  expect_true("TraitID" %in% names(info))
  expect_true("TraitName" %in% names(info))
  # Should have SLA (3115), LNC (14), and life form (341)
  expect_true(3115 %in% info$TraitID)
  expect_true(14 %in% info$TraitID)
})

test_that("remove_experiments removes treatment datasets via DataID 327", {
  f <- test_path("fixtures", "sample_try.txt")
  data <- read_try(f)

  # Dataset 300 has an Exposition row (DataID 327)
  expect_true(300 %in% data$DatasetID)

  cleaned <- remove_experiments(data)
  expect_false(300 %in% cleaned$DatasetID)
  # Other datasets still present
  expect_true(100 %in% cleaned$DatasetID)
})

test_that("remove_duplicates removes records where OrigObsDataID != ObsDataID", {
  f <- test_path("fixtures", "sample_try.txt")
  data <- read_try(f)

  # Fixture has a duplicate row (ObsDataID 10015 with OrigObsDataID 10011)
  expect_true(10015 %in% data$ObsDataID)

  cleaned <- remove_duplicates(data)
  expect_false(10015 %in% cleaned$ObsDataID)
  # Original record still present
  expect_true(10011 %in% cleaned$ObsDataID)
})

test_that("split_traits separates qualitative and quantitative", {
  f <- test_path("fixtures", "sample_try.txt")
  data <- read_try(f)
  data <- remove_experiments(data)

  result <- split_traits(data, qualitative_ids = c(341))
  expect_named(result, c("quantitative", "qualitative"))
  expect_true(nrow(result$qualitative) > 0)
  expect_true(all(result$qualitative$TraitID == 341))
  expect_false(341 %in% result$quantitative$TraitID)
})

test_that("split_traits with no qualitative IDs returns all as quantitative", {
  f <- test_path("fixtures", "sample_try.txt")
  data <- read_try(f)

  result <- split_traits(data)
  # Metadata rows (TraitID = NA) are dropped, so fewer rows than input
  n_traits <- sum(!is.na(data$TraitID))
  expect_equal(nrow(result$quantitative), n_traits)
  expect_equal(nrow(result$qualitative), 0)
  # No NA TraitIDs in output
  expect_false(any(is.na(result$quantitative$TraitID)))
})

test_that("split_traits drops metadata rows (NA TraitID)", {
  f <- test_path("fixtures", "sample_try.txt")
  data <- read_try(f)
  data <- remove_experiments(data)

  # Fixture has metadata rows (Latitude DataID=59, Longitude DataID=60)
  expect_true(any(is.na(data$TraitID)))

  result <- split_traits(data, qualitative_ids = c(341))
  # No NA TraitIDs in either output
  expect_false(any(is.na(result$quantitative$TraitID)))
  expect_false(any(is.na(result$qualitative$TraitID)))
})

test_that("clean_quantitative filters by ErrorRisk", {
  f <- test_path("fixtures", "sample_try.txt")
  data <- read_try(f)
  data <- remove_experiments(data)
  split <- split_traits(data, qualitative_ids = c(341))

  cleaned <- clean_quantitative(split$quantitative, max_error_risk = 4)

  # Observation 1003 has ErrorRisk = 5 for SLA, should be removed
  high_risk <- cleaned |> dplyr::filter(.data$ErrorRisk >= 4)
  expect_equal(nrow(high_risk), 0)
})

# Observation-level removal (0.2.0) -----------------------------------------
exp_fixture <- function() {
  tibble::tibble(
    DatasetID = 1L,
    ObservationID = c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 6L),
    DataID = c(3115L, 327L, 3115L, 327L, 3115L, 327L, 3115L, 319L, 3115L, 443L, 3115L),
    DataName = c("SLA", "Treatment: Exposition", "SLA", "Treatment: Exposition",
                 "SLA", "Treatment: Exposition", "SLA", "Treatment light",
                 "SLA", "Canopy position: sun vers. Shade leaf qualifier", "SLA"),
    OrigValueStr = c("20", "Natural environment.", "21", "Greenhouse",
                     "22", "CG", "23", NA, "24", "sun", "25")
  )
}

test_that("remove_experiments keeps natural observations of a dataset with experiments", {
  out <- remove_experiments(exp_fixture())
  # 1: natural; 5: canopy position is not a treatment; 6: no treatment field
  expect_setequal(unique(out$ObservationID), c(1L, 5L, 6L))
})

test_that("remove_experiments removes ambiguous and empty treatment values", {
  out <- remove_experiments(exp_fixture())
  # 3: ambiguous code "CG"; 4: empty treatment value
  expect_false(any(c(2L, 3L, 4L) %in% out$ObservationID))
  flagged <- attr(out, "experimental_values")
  expect_true(all(c("greenhouse", "cg", "") %in% flagged$value))
})

test_that("remove_experiments accepts extra natural spellings", {
  out <- remove_experiments(exp_fixture(), natural_values = c(default_natural_values(), "CG"))
  expect_true(3L %in% out$ObservationID)
})

test_that("remove_experiments level = 'dataset' reproduces the pre-0.2.0 behaviour", {
  out <- remove_experiments(exp_fixture(), level = "dataset")
  expect_equal(nrow(out), 0L)
})
