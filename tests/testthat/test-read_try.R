test_that("read_try reads and filters by species", {
  f <- test_path("fixtures", "sample_try.txt")

  # Read all
  all_data <- read_try(f, species = NULL)
  expect_s3_class(all_data, "tbl_df")
  expect_true(nrow(all_data) > 0)

  # Filter by species
  filtered <- read_try(f, species = c("Papaver rhoeas"))
  expect_true(all(
    filtered$SpeciesName == "Papaver rhoeas" | filtered$AccSpeciesName == "Papaver rhoeas"
  ))
  expect_true(nrow(filtered) < nrow(all_data))
})

test_that("read_try reads from directory", {
  dir_path <- test_path("fixtures")
  data <- read_try(dir_path, species = c("Papaver rhoeas"))
  expect_s3_class(data, "tbl_df")
  expect_true(nrow(data) > 0)
})

test_that("read_try errors on missing files", {
  expect_error(read_try("/nonexistent/file.txt"), "not found")
})

test_that("list_try_files works", {
  dir_path <- test_path("fixtures")
  result <- list_try_files(dir_path)
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) >= 1)
  expect_named(result, c("file", "size"))
})

test_that("list_try_files errors on missing directory", {
  expect_error(list_try_files("/nonexistent"), "does not exist")
})
