test_that("trait_map creates a valid mapping", {
  m <- trait_map(
    SLA = c("Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded",
            "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included"),
    N_percent = "Leaf nitrogen (N) content per leaf dry mass"
  )

  expect_s3_class(m, "try_trait_map")
  expect_true("original" %in% names(m))
  expect_true("short_name" %in% names(m))
  expect_equal(nrow(m), 3)
  expect_equal(sum(m$short_name == "SLA"), 2)
})

test_that("rename_traits applies mapping correctly", {
  f <- test_path("fixtures", "sample_try.txt")
  data <- read_try(f, species = c("Papaver rhoeas"))

  m <- trait_map(
    SLA = "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded",
    N_percent = "Leaf nitrogen (N) content per leaf dry mass"
  )

  renamed <- rename_traits(data, m, warn_unmapped = FALSE)
  trait_names <- unique(renamed$TraitName[!is.na(renamed$TraitName)])
  expect_true("SLA" %in% trait_names)
  expect_true("N_percent" %in% trait_names)
})

test_that("rename_traits keeps unmapped traits as-is", {
  df <- tibble::tibble(TraitName = c("Trait A", "Trait B", "Trait C"))
  m <- trait_map(short_a = "Trait A")

  result <- rename_traits(df, m, warn_unmapped = FALSE)
  expect_equal(result$TraitName, c("short_a", "Trait B", "Trait C"))
})

test_that("suggest_trait_map returns a template", {
  df <- tibble::tibble(TraitName = c("Trait A", "Trait B", "Trait A", NA))
  template <- suggest_trait_map(df)

  expect_equal(nrow(template), 2)
  expect_true(all(is.na(template$short_name)))
  expect_equal(template$original, c("Trait A", "Trait B"))
})

test_that("default_trait_map loads the shipped mapping", {
  m <- default_trait_map()
  expect_s3_class(m, "try_trait_map")
  expect_true(nrow(m) > 0)
  expect_true("SLA" %in% m$short_name)
  expect_true("N_percent" %in% m$short_name)
})

test_that("trait_map errors with no arguments", {
  expect_error(trait_map(), "at least one mapping")
})
