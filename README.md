# tidyTRY

An R package for reading, cleaning, and processing plant trait data from the [TRY database](https://www.try-db.org/).

## Why?

TRY data exports are large (often 1-10+ GB), messy, and require substantial cleaning for every project. `tidyTRY` provides a modular pipeline that handles the common pain points so you don't have to write a new script each time.

## Features

- **Memory-efficient reading** of large TRY files using chunked processing
- **Species filtering** at read time (critical for multi-GB files)
- **Taxonomy resolution** via TNRS (online) or WorldFlora (offline)
- **Experiment removal** (automatic detection of treatment datasets)
- **Trait type splitting** (quantitative vs qualitative)
- **Error risk filtering** for quantitative trait quality control
- **Trait renaming** with a flexible many-to-one mapping API (e.g., merge 3 SLA variants into "SLA")
- **Coordinate extraction** from TRY's embedded location metadata
- **Diagnostic summaries** and **distribution plots** for visual QC

## Installation

```r
# Install from GitHub
devtools::install_github("billurbektas/tidyTRY")
```

## Before You Start

1. **Request and download data from TRY** at <https://www.try-db.org/>
2. Place the `.txt` export files in a folder (e.g., `data/try/`)
3. Know which species you want to analyze

## Quick Start

For the common workflow, `process_try()` chains everything together in one call:

```r
library(tidyTRY)

my_species <- c("Papaver rhoeas", "Centaurea cyanus", "Trifolium arvense")

result <- process_try(
  files = "data/try/",
  species = my_species,
  qualitative_ids = c(341),
  trait_map = default_trait_map(),
  resolve_method = "tnrs",
  max_error_risk = 4,
  extract_location = TRUE
)

# Access results
result$quantitative      # cleaned quantitative trait data
result$qualitative       # qualitative trait data
result$taxonomy          # species name resolution
result$coordinates       # extracted lat/lon
result$diagnostics       # trait availability matrix
result$units             # trait units
result$trait_info        # all trait IDs and names found
```

## Step-by-Step Workflow

### Step 1: Read TRY Data

TRY files can be very large (1-10+ GB). `read_try()` uses chunked reading to filter by species without loading everything into memory.

```r
library(tidyTRY)

# See what files you have and their sizes
list_try_files("data/try/")

# Read and filter by species
my_species <- c(
  "Papaver rhoeas",
  "Centaurea cyanus",
  "Trifolium arvense",
  "Agrostemma githago"
)

dat <- read_try("data/try/", species = my_species)
```

### Step 2: Resolve Taxonomy

Species names in TRY may not match current accepted taxonomy. Use `resolve_species()` to harmonize names. Two backends are supported:

```r
# Online via TNRS (default) - uses WCVP + WFO
taxonomy <- resolve_species(my_species, method = "tnrs")
taxonomy

# Offline via WorldFlora (requires one-time backbone download)
# Download from: https://www.worldfloraonline.org/downloadData
taxonomy <- resolve_species(my_species, method = "worldflora",
                            wfo_backbone = "path/to/classification.csv")
```

### Step 3: Inspect and Clean Traits

```r
# See what traits are in your data
trait_info <- get_trait_info(dat)
trait_info

# Remove experimental datasets (treatment studies)
dat <- remove_experiments(dat)

# Decide which traits are qualitative (you must inspect trait_info first!)
# For example, trait 341 = "Plant life form (Raunkiaer life form)"
quali_ids <- c(341)

# Split into quantitative and qualitative
split <- split_traits(dat, qualitative_ids = quali_ids)

# Clean quantitative traits: remove high error risk observations
quanti <- clean_quantitative(split$quantitative, max_error_risk = 4)
```

### Step 4: Rename Traits

TRY trait names are long and verbose. You can rename them to short codes.

```r
# See what trait names are available
suggest_trait_map(quanti)

# Use the built-in default mapping
quanti <- rename_traits(quanti, default_trait_map())

# Or define your own (many-to-one mapping supported)
my_map <- trait_map(
  SLA = c(
    "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded",
    "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included"
  ),
  N_percent = "Leaf nitrogen (N) content per leaf dry mass",
  LDMC = "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)",
  seed_mass = "Seed dry mass"
)
quanti <- rename_traits(quanti, my_map)

# You can also save/load mappings as CSV
write.csv(my_map, "my_trait_map.csv", row.names = FALSE)
my_map <- read_trait_map("my_trait_map.csv")
```

### Step 5: Extract Location Data

TRY stores latitude/longitude as metadata rows within the export. Extract them:

```r
coords <- extract_coordinates(dat)
head(coords)

# Optionally extract climate zones (requires terra + sf + a raster)
library(terra)
kg_raster <- rast("data/koppen_geiger_0p1.tif")
kg_legend <- read.csv2("data/koppen_geiger_legend.txt")
coords <- extract_climate_zones(coords, kg_raster, legend = kg_legend)
```

### Step 6: Diagnostics

```r
# Trait availability: how many observations per species per trait?
avail <- trait_availability(quanti)
avail

# Summary statistics
summ <- trait_summary(quanti)
summ

# Visual check: density + boxplots per species
plot_trait_distributions(quanti, output_file = "trait_distributions.pdf")
```

## Ellenberg Indicators and Dispersal Traits

The package also supports cleaning ecological indicator values and dispersal traits from [floraveg.eu](https://floraveg.eu/download/). Data can be **downloaded automatically** -- no need to manually visit the website.

### Download Data

```r
# Download datasets from floraveg.eu (cached, won't re-download)
download_floraveg("ellenberg_disturbance")  # Ellenberg + disturbance combined
download_floraveg("dispersal")              # Lososova et al. 2023 (v2)
download_floraveg("ellenberg")              # Ellenberg values only
download_floraveg("disturbance")            # Disturbance values only
download_floraveg("life_form")              # Life form classifications

# Cache permanently in your project
download_floraveg("ellenberg_disturbance", dest_dir = "data/")
```

### Ellenberg Indicators

```r
# Auto-download + clean in one step
indicators <- read_indicators(species = my_species)

# Or with more control
taxonomy <- resolve_species(my_species)
indicators <- read_indicators(
  species = taxonomy,
  extra_matches = data.frame(
    species_source = "Taraxacum sect. Taraxacum",
    species_TNRS = "Taraxacum officinale"
  )
)
```

### Dispersal Traits

```r
# Auto-download + clean
dispersal <- read_dispersal(species = my_species)

# Or from a local file
dispersal <- read_dispersal(
  file = "data/Lososova_et_al_2023_Dispersal.xlsx",
  species = my_species
)
```

## Advanced: Handling Special Traits

Some traits like maturity (TraitID 155) need project-specific cleaning that goes beyond what the package automates. Here's an example of how to handle maturity data after using tidyTRY:

```r
# After reading and cleaning with tidyTRY, extract maturity separately
maturity <- dat |>
  dplyr::filter(TraitID == 155, OrigValueStr != "") |>
  dplyr::select(DatasetID, ObservationID, AccSpeciesName, OrigValueStr) |>
  dplyr::mutate(
    Value = dplyr::case_when(
      OrigValueStr %in% c("3 months to 1 year", "<1", "< 3 months") ~ "0",
      OrigValueStr %in% c("1 to 2 years", "1-2 yrs") ~ "1-2",
      OrigValueStr %in% c("2 to 6 years", "2-5 yrs") ~ "2-6",
      # ... add your own project-specific mappings
      .default = OrigValueStr
    )
  )
```

## License

MIT
