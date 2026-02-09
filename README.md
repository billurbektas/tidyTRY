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

## Quick Start

```r
library(tidyTRY)

# Full pipeline in one call
result <- process_try(
  files = "data/try/",
  species = c("Papaver rhoeas", "Centaurea cyanus", "Trifolium arvense"),
  qualitative_ids = c(341),
  trait_map = default_trait_map(),
  resolve_method = "tnrs"
)

# Access cleaned data
result$quantitative
result$diagnostics
```

## Step-by-Step Usage

```r
library(tidyTRY)

# 1. See what files you have
list_try_files("data/try/")

# 2. Read data, filtering by species
dat <- read_try("data/try/", species = c("Papaver rhoeas", "Centaurea cyanus"))

# 3. Resolve taxonomy
taxonomy <- resolve_species(c("Papaver rhoeas", "Centaurea cyanus"))

# 4. Inspect available traits
get_trait_info(dat)

# 5. Remove experimental datasets
dat <- remove_experiments(dat)

# 6. Split into quantitative and qualitative
split <- split_traits(dat, qualitative_ids = c(341))

# 7. Clean quantitative traits (ErrorRisk < 4)
quanti <- clean_quantitative(split$quantitative)

# 8. Discover and rename traits
suggest_trait_map(quanti)  # see what's available
quanti <- rename_traits(quanti, default_trait_map())

# 9. Extract coordinates
coords <- extract_coordinates(dat)

# 10. Diagnostics
trait_availability(quanti)
trait_summary(quanti)
plot_trait_distributions(quanti, output_file = "trait_check.pdf")
```

## Trait Renaming

The package ships a default mapping for common traits (SLA, LDMC, N%, etc.). You can also define your own:

```r
# Custom mapping (many-to-one supported)
my_map <- trait_map(
  SLA = c(
    "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded",
    "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included"
  ),
  N_percent = "Leaf nitrogen (N) content per leaf dry mass"
)

dat <- rename_traits(dat, my_map)

# Or load from a CSV file
my_map <- read_trait_map("my_mapping.csv")
```

## Taxonomy Resolution

Two backends are supported:

```r
# TNRS (online, default) - uses WCVP + WFO
resolved <- resolve_species(species, method = "tnrs")

# WorldFlora (offline after one-time download)
resolved <- resolve_species(species, method = "worldflora",
                            wfo_backbone = "path/to/classification.csv")
```

## License

MIT
