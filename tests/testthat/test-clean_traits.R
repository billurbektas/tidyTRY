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

test_that("remove_experiments removes treatment datasets", {
  f <- test_path("fixtures", "sample_try.txt")
  data <- read_try(f)

  # Dataset 300 has a Treatment row
  expect_true(300 %in% data$DatasetID)

  cleaned <- remove_experiments(data)
  expect_false(300 %in% cleaned$DatasetID)
  # Other datasets still present
  expect_true(100 %in% cleaned$DatasetID)
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
  expect_equal(nrow(result$quantitative), nrow(data))
  expect_equal(nrow(result$qualitative), 0)
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
