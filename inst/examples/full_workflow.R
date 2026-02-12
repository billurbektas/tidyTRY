# ==============================================================================
# Full tidyTRY Workflow Example
#
# This script demonstrates the complete pipeline for cleaning and processing
# plant trait data from multiple sources:
#   1. Process raw TRY database exports (.txt files)
#   2. Download ecological indicators from floraveg.eu
#   3. Download dispersal traits from floraveg.eu
#   4. Filter by climate zone
#   5. Create individual-level and species-level trait datasets
#   6. (Optional) Impute missing trait values using random forest
#
# Before running:
#   - Request and download data from https://www.try-db.org/
#   - Place the .txt export files in a folder (e.g., "data/try/")
#   - Prepare a character vector of your target species
#
# Install tidyTRY:
#   devtools::install_github("billurbektas/tidyTRY")
# ==============================================================================

library(tidyverse)
library(tidyTRY)

# ==============================================================================
# 1. DEFINE YOUR SPECIES LIST
# ==============================================================================

# Option A: From a vegetation survey CSV
# veg_raw = read_csv("data/vegetation/my_survey.csv")
# species_list = veg_raw %>%
#   pull(species_column) %>%
#   unique() %>%
#   sort()

# Option B: Define manually
species_list = c(
  "Papaver rhoeas",
  "Centaurea cyanus",
  "Trifolium arvense",
  "Achillea millefolium",
  "Dactylis glomerata"
)

cat("Target species:", length(species_list), "\n")

# ==============================================================================
# 2. INSPECT AVAILABLE TRAITS (run once to explore your data)
# ==============================================================================

try_path = "data/try/"  # <-- path to your TRY .txt files

# List available TRY files and their sizes
try_files = list_try_files(try_path)
print(try_files)

# Read a small sample to see what traits are available
try_sample = read_try(try_path, species = species_list[1:5], progress = FALSE)

# What traits are in the data?
trait_info = get_trait_info(try_sample)
print(trait_info, n = 50)

# Get a template for building your trait map
suggest_trait_map(try_sample) %>% print(n = 30)

# ==============================================================================
# 3. DEFINE TRAIT MAPPING
# ==============================================================================
# Map verbose TRY trait names to short codes. Many-to-one mapping is supported
# (e.g., multiple SLA definitions -> "SLA"). Use suggest_trait_map() output
# above to see what names are in YOUR data.

trait_mapping = trait_map(
  N_percent = c(
    "Leaf nitrogen (N) content per leaf dry mass",
    "Leaf nitrogen content per leaf dry mass"
  ),
  C_percent = c(
    "Leaf carbon (C) content per leaf dry mass",
    "Leaf carbon content per leaf dry mass"
  ),
  LDMC = c(
    "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)",
    "Leaf dry mass per leaf fresh mass (LDMC)"
  ),
  SLA = c(
    "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded",
    "Leaf area per leaf dry mass (SLA or 1/LMA): petiole excluded",
    "Leaf area per leaf dry mass (SLA or 1/LMA): petiole included",
    "Leaf area per leaf dry mass (SLA)",
    "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded",
    "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included",
    "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA) petiole, rhachis and midrib excluded"
  ),
  LA = c(
    "Leaf area (in case of compound leaves: leaflet, undefined if petiole is in- or excluded)",
    "Leaf area",
    "Leaf area (in case of compound leaves: leaf)",
    "Leaf area (in case of compound leaves: leaflet, petiole excluded)",
    "Leaf area (in case of compound leaves: leaf, petiole excluded)",
    "Leaf area (in case of compound leaves: leaflet, petiole included)",
    "Leaf area (in case of compound leaves: leaf, petiole included)",
    "Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole is in- or excluded)",
    "Leaf area (in case of compound leaves: leaf, undefined if petiole in- or excluded)"
  ),
  vegetative_height = c(
    "Plant height vegetative",
    "Plant vegetative height"
  ),
  reproductive_height = c(
    "Plant height generative",
    "Plant generative height"
  ),
  seed_mass = c(
    "Seed dry mass",
    "Seed mass"
  ),
  flowering_phenology = c(
    "Plant reproductive phenology timing (flowering time)"
  )
)

# ==============================================================================
# 4. PROCESS TRY DATA
# ==============================================================================
# process_try() chains the full pipeline: read -> resolve taxonomy -> remove
# experiments -> remove duplicates -> split traits -> clean -> rename

try_result = process_try(
  files = try_path,
  species = species_list,
  qualitative_ids = c(335),        # <-- TraitIDs that are qualitative (inspect trait_info!)
  trait_map = trait_mapping,
  resolve_taxonomy = TRUE,
  resolve_method = "tnrs",         # "tnrs" (online) or "worldflora" (offline)
  max_error_risk = 4,              # remove observations with ErrorRisk >= 4
  extract_location = TRUE,
  chunk_size = 100000L
)

# Access results
try_quanti = try_result$quantitative
cat("Trait observations:", nrow(try_quanti), "\n")
cat("Species with TRY data:", n_distinct(try_quanti$AccSpeciesName), "\n")

# ==============================================================================
# 5. EXTRACT CLIMATE ZONES
# ==============================================================================
# Uses the bundled Koppen-Geiger raster (Beck et al. 2023) -- no external files
# needed. You can also supply your own raster via the climate_raster argument.

coords_with_climate = extract_climate_zones(try_result$coordinates)

# Merge climate zones back to trait data
try_quanti = try_quanti %>%
  left_join(
    coords_with_climate %>% select(ObservationID, Climate_code = climate_code),
    by = "ObservationID"
  )

# ==============================================================================
# 6. DOWNLOAD ECOLOGICAL INDICATORS FROM FLORAVEG.EU
# ==============================================================================
# Downloads and caches indicator data automatically. Resolves YOUR species
# taxonomy and matches against the floraveg.eu database.

indicators = read_indicators(
  species = try_result$taxonomy,    # pass taxonomy from process_try()
  indicators = c("Light", "Moisture", "Nutrients", "Disturbance.Severity")
)

# Rename for downstream use
indicators = indicators %>%
  rename(
    species = species_original,
    species_TNRS = species_resolved,
    disturbance = Disturbance.Severity
  ) %>%
  select(species, species_TNRS, Light, Moisture, Nutrients, disturbance)

# ==============================================================================
# 7. DOWNLOAD DISPERSAL TRAITS FROM FLORAVEG.EU
# ==============================================================================

dispersal = read_dispersal(
  species = try_result$taxonomy
)

dispersal = dispersal %>%
  rename(
    species = species_original,
    species_TNRS = species_resolved,
    dispersal = dispersal_distance_class
  ) %>%
  select(species, species_TNRS, dispersal)

# ==============================================================================
# 8. FILTER BY CLIMATE ZONE (optional)
# ==============================================================================
# Exclude tropical and arid climates to focus on temperate observations.
# Adjust or skip this step depending on your study region.

excluded_climates = c("Af", "Am", "Aw", "BWh", "BWk", "BSh", "BSk")

try_filtered = try_quanti %>%
  filter(!Climate_code %in% excluded_climates | is.na(Climate_code))

cat("After climate filtering:", nrow(try_filtered), "observations\n")

# ==============================================================================
# 9. CREATE INDIVIDUAL-LEVEL OUTPUT
# ==============================================================================
# One row per trait observation -- useful for intraspecific trait variation (ITV)
# analyses, mixed models, etc.

try_individual = try_filtered %>%
  select(
    species = SpeciesName,
    species_TNRS = AccSpeciesName,
    TraitName,
    Value = StdValue,
    ObservationID,
    Climate_code,
    Dataset = DatasetID,
    Unit = UnitName
  ) %>%
  filter(!is.na(Value))

write_csv(try_individual, "output/try_traits_individual.csv")

# ==============================================================================
# 10. CREATE SPECIES-LEVEL SUMMARIES
# ==============================================================================

try_species = try_individual %>%
  group_by(species, species_TNRS, TraitName) %>%
  summarize(
    Mean = mean(Value, na.rm = TRUE),
    Var = var(Value, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = TraitName,
    values_from = c(Mean, Var, n_obs),
    names_glue = "{.value}_{TraitName}"
  )

write_csv(try_species, "output/try_traits_species.csv")

# ==============================================================================
# 11. MERGE ALL TRAIT SOURCES
# ==============================================================================

traits = dispersal %>%
  left_join(try_species, by = c("species", "species_TNRS")) %>%
  left_join(indicators, by = c("species", "species_TNRS"))

write_csv(traits, "output/traits_raw.csv")

# ==============================================================================
# 12. IMPUTE MISSING TRAIT VALUES (optional)
# ==============================================================================
# Random forest imputation using missForest. This requires a custom function
# (not part of tidyTRY) since imputation strategies are project-specific.
#
# Below is a minimal example using the missForest package directly:
#
# library(missForest)
#
# # Select numeric columns to impute
# cols_to_impute = c("Mean_seed_mass", "Mean_LA", "Mean_SLA", "Mean_LDMC",
#                     "Mean_vegetative_height", "Mean_reproductive_height",
#                     "Mean_N_percent", "Mean_C_percent")
#
# trait_matrix = traits %>% select(all_of(cols_to_impute))
# imputed = missForest(as.data.frame(trait_matrix), maxiter = 100, ntree = 500)
#
# traits[, cols_to_impute] = imputed$ximp
# write_csv(traits, "output/traits_imputed.csv")

# ==============================================================================
# 13. DIAGNOSTIC PLOTS (optional)
# ==============================================================================
# Visual QC: density + boxplots per species per trait

# plot_trait_distributions(try_individual, output_file = "output/trait_distributions.pdf")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n=== Summary ===\n")
cat("Species in input list:", length(species_list), "\n")
cat("Species with TRY data:", n_distinct(try_species$species), "\n")
cat("Species with indicators:", n_distinct(indicators$species), "\n")
cat("Species with dispersal:", n_distinct(dispersal$species), "\n")
cat("Individual trait observations:", nrow(try_individual), "\n")

cat("\nTrait coverage (% species with data):\n")
traits %>%
  summarize(across(-c(species, species_TNRS), ~ round(100 * mean(!is.na(.)), 1))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "percent_complete") %>%
  arrange(desc(percent_complete)) %>%
  print(n = 25)
