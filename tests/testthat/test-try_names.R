test_that("try_species_names returns the distinct name combinations", {
  f <- test_path("fixtures", "sample_try.txt")
  nm <- try_species_names(f, progress = FALSE)
  expect_named(nm, c("AccSpeciesID", "AccSpeciesName", "SpeciesName", "n_rows"))
  expect_equal(nrow(nm), 4L)
  expect_equal(sum(nm$n_rows), nrow(read_try(f, progress = FALSE)))
})

test_that("read_try filters by AccSpeciesID", {
  f <- test_path("fixtures", "sample_try.txt")
  by_id <- read_try(f, species_ids = 5002L, progress = FALSE)
  expect_true(nrow(by_id) > 0)
  expect_true(all(by_id$AccSpeciesID == 5002L))
})

test_that("assign_accepted_names prefers the contributor's SpeciesName", {
  data <- tibble::tibble(
    SpeciesName = c("Festuca pratensis", "Festuca ovina", "Unknown name"),
    AccSpeciesName = c("Festuca ovina", "Festuca ovina", "Festuca ovina")
  )
  taxonomy <- tibble::tibble(
    submitted_name = c("Festuca pratensis", "Festuca ovina"),
    accepted_name = c("Lolium pratense", "Festuca ovina")
  )
  out <- assign_accepted_names(data, taxonomy)
  expect_equal(out$accepted_name, c("Lolium pratense", "Festuca ovina", "Festuca ovina"))
  expect_equal(out$name_source, c("SpeciesName", "SpeciesName", "AccSpeciesName"))
})
