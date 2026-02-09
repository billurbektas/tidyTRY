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
  expect_named(result, c("submitted_name", "accepted_name", "matched_name", "source", "score"))
  expect_equal(nrow(result), 2)
})

test_that("resolve_species errors with worldflora and no backbone", {
  skip_if_not_installed("WorldFlora")

  expect_error(
    resolve_species("Papaver rhoeas", method = "worldflora"),
    "backbone"
  )
})
