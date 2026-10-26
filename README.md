# tidyTRY

<!-- badges: start -->
[![R-CMD-check](https://github.com/billurbektas/tidyTRY/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/billurbektas/tidyTRY/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

An R package for reading, cleaning, and processing plant trait data from the [TRY database](https://www.try-db.org/).

## Why?

TRY data exports are large (often 1-10+ GB), messy, and require substantial cleaning for every project. `tidyTRY` provides a modular pipeline that handles the common pain points so you don't have to write a new script each time.

## Features

- **Memory-efficient reading** of large TRY files using chunked processing
- **Species filtering** at read time, by name or by TRY species ID (critical for multi-GB files)
- **Taxonomy resolution** via TNRS (online; legitimate names only, ties flagged for review) or WorldFlora (offline)
- **Experiment removal** per observation, from TRY's treatment fields (DataID 327 and the other "Treatment" fields)
- **Duplicate removal** (via OrigObsDataID flagging)
- **Trait type splitting** (quantitative vs qualitative)
- **Error risk filtering** for quantitative trait quality control
- **Trait renaming** with a flexible many-to-one mapping API (e.g., merge 3 SLA variants into "SLA")
- **Coordinate extraction** via DataID 59/60 (with DataName fallback)
- **Climate zone classification** with bundled Koppen-Geiger raster (Beck et al. 2023)
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

#### Reading by TRY species ID

Filtering by name keeps rows whose `SpeciesName` or `AccSpeciesName` equals one
of your names. TRY's `AccSpeciesName` is TRY's own accepted name and can merge
species: records submitted as *Festuca pratensis* are filed under "Festuca
ovina". The safer route resolves the TRY names first, reads by TRY species ID,
and then keeps the rows whose own name resolves to one of your taxa
(`process_try()` does this for you):

```r
try_names <- try_species_names("data/try/")
taxonomy <- resolve_species(c(my_species, try_names$SpeciesName, try_names$AccSpeciesName))
study <- taxonomy$accepted_name[taxonomy$submitted_name %in% my_species]
lookup <- setNames(taxonomy$accepted_name, taxonomy$submitted_name)
ids <- unique(try_names$AccSpeciesID[lookup[try_names$SpeciesName] %in% study |
                                     lookup[try_names$AccSpeciesName] %in% study])
dat <- read_try("data/try/", species_ids = ids)
dat <- assign_accepted_names(dat, taxonomy)
dat <- dat[dat$accepted_name %in% study, ]
```

### Step 2: Resolve Taxonomy

Species names in TRY may not match current accepted taxonomy. Use `resolve_species()` to harmonize names. Two backends are supported.

With TNRS, all candidate matches are requested and only legitimate names (status "Accepted" or "Synonym") are used. For a name with homonyms, TNRS's own "best" match can be an illegitimate name by another author: before version 0.2.0, "Festuca pratensis" resolved to *Festuca ovina* instead of *Lolium pratense*. See `NEWS.md`.

```r
# Online via TNRS (default) - WCVP first, WFO as flagged fallback
taxonomy <- resolve_species(my_species, method = "tnrs")
taxonomy

# Names that need a decision by hand: ties between legitimate names,
# no legitimate candidate, or a name taken from the fallback source
taxonomy[taxonomy$needs_review, c("submitted_name", "review_reason")]

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

# Remove observations made under experimental conditions (treatment fields
# such as DataID 327 with a value other than a clearly natural one)
dat <- remove_experiments(dat)

# Remove duplicate records (flagged via OrigObsDataID)
dat <- remove_duplicates(dat)

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

### Step 5: Extract Location Data and Climate Zones

TRY stores latitude/longitude as metadata rows within the export. Extract them:

```r
coords <- extract_coordinates(dat)
head(coords)

# Assign Koppen-Geiger climate zones (raster + legend are bundled in the package)
coords <- extract_climate_zones(coords)
head(coords[, c("ObservationID", "Latitude", "Longitude", "climate_code", "climate_description")])

# Or supply your own raster
library(terra)
my_raster <- rast("path/to/my_climate_raster.tif")
coords <- extract_climate_zones(coords, climate_raster = my_raster, legend = my_legend)
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

## References

The cleaning workflow in this package is inspired by:

> Bjorkman, A.D., Myers-Smith, I.H., Elmendorf, S.C. et al. (2018). Plant functional trait change across a warming tundra biome. *Nature*, 562, 57--62. https://doi.org/10.1038/s41586-018-0563-7

Climate zone classification uses the Koppen-Geiger maps from:

> Beck, H.E., McVicar, T.R., Vergopolan, N. et al. (2023). High-resolution (1 km) Koppen-Geiger maps for 1901--2099 based on constrained CMIP6 projections. *Scientific Data*, 10, 724. https://doi.org/10.1038/s41597-023-02549-6

## License

MIT
