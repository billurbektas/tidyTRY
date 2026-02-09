test_that("trait_availability produces species x trait matrix", {
  f <- test_path("fixtures", "sample_try.txt")
  data <- read_try(f)
  data <- remove_experiments(data)
  split <- split_traits(data, qualitative_ids = c(341))
  quanti <- clean_quantitative(split$quantitative, max_error_risk = 4)

  avail <- trait_availability(quanti)
  expect_s3_class(avail, "tbl_df")
  expect_true("AccSpeciesName" %in% names(avail))
  # Should have at least one trait column besides the species column
  expect_true(ncol(avail) > 1)
})

test_that("trait_summary computes statistics", {
  f <- test_path("fixtures", "sample_try.txt")
  data <- read_try(f)
  data <- remove_experiments(data)
  split <- split_traits(data, qualitative_ids = c(341))
  quanti <- clean_quantitative(split$quantitative, max_error_risk = 4)

  summ <- trait_summary(quanti)
  expect_s3_class(summ, "tbl_df")
  expect_true(all(c("n", "mean", "sd", "median", "min", "max") %in% names(summ)))
  expect_true(all(summ$n > 0))
})
