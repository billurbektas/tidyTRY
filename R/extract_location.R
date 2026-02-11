#' Extract coordinates from TRY data
#'
#' Extracts latitude and longitude from TRY data. TRY stores location
#' information as covariate rows: Latitude is DataID 59 and Longitude is
#' DataID 60, with standardized values in the `StdValue` column (see TRY 6.0
#' Release Notes, section c). Falls back to `DataName` string matching for
#' older exports that may use different DataIDs.
#'
#' @param data A data frame of TRY data (as returned by [read_try()]).
#' @param corrections Optional data frame with manual coordinate corrections.
#'   Must have columns `ObservationID` (or `DatasetID`) plus `Latitude`
#'   and/or `Longitude`. These override extracted values.
#' @param data_id_col Name of the DataID column. Defaults to `"DataID"`.
#' @param data_name_col Name of the DataName column. Defaults to `"DataName"`.
#' @param std_value_col Name of the StdValue column. Defaults to `"StdValue"`.
#' @param orig_value_col Name of the OrigValueStr column. Defaults to `"OrigValueStr"`.
#' @param observation_col Name of the ObservationID column. Defaults to `"ObservationID"`.
#' @param dataset_col Name of the DatasetID column. Defaults to `"DatasetID"`.
#'
#' @return A tibble with columns `DatasetID`, `Dataset`, `ObservationID`,
#'   `Latitude`, and `Longitude`.
#' @export
extract_coordinates <- function(data,
                                corrections = NULL,
                                data_id_col = "DataID",
                                data_name_col = "DataName",
                                std_value_col = "StdValue",
                                orig_value_col = "OrigValueStr",
                                observation_col = "ObservationID",
                                dataset_col = "DatasetID") {
  # Primary: use DataID 59 (Latitude) and 60 (Longitude) per TRY 6.0 spec
  has_data_id <- data_id_col %in% names(data)

  if (has_data_id) {
    loc_data <- data |>
      dplyr::filter(.data[[data_id_col]] %in% c(59L, 60L)) |>
      dplyr::select(
        dplyr::all_of(c(dataset_col, "Dataset", observation_col, data_id_col,
                         orig_value_col, std_value_col))
      )
  } else {
    loc_data <- data[0, ]
  }

  # Fallback: match on DataName if DataID approach found nothing
  if (nrow(loc_data) == 0) {
    loc_data <- data |>
      dplyr::filter(
        stringr::str_detect(
          .data[[data_name_col]],
          stringr::regex("longitude|latitude", ignore_case = TRUE)
        )
      ) |>
      dplyr::select(
        dplyr::all_of(c(dataset_col, "Dataset", observation_col, data_name_col,
                         orig_value_col, std_value_col))
      )
  }

  if (nrow(loc_data) == 0) {
    cli::cli_warn("No location data found.")
    return(tibble::tibble(
      DatasetID = integer(),
      Dataset = character(),
      ObservationID = integer(),
      Latitude = numeric(),
      Longitude = numeric()
    ))
  }

  # Determine coordinate type: prefer DataID, fall back to DataName
  if (has_data_id && data_id_col %in% names(loc_data)) {
    loc_data <- loc_data |>
      dplyr::mutate(
        value = dplyr::if_else(
          !is.na(.data[[std_value_col]]),
          as.character(.data[[std_value_col]]),
          .data[[orig_value_col]]
        ),
        coord_type = dplyr::case_when(
          .data[[data_id_col]] == 59L ~ "Latitude",
          .data[[data_id_col]] == 60L ~ "Longitude"
        )
      )
  } else {
    loc_data <- loc_data |>
      dplyr::mutate(
        value = dplyr::if_else(
          !is.na(.data[[std_value_col]]),
          as.character(.data[[std_value_col]]),
          .data[[orig_value_col]]
        ),
        coord_type = dplyr::case_when(
          stringr::str_detect(.data[[data_name_col]], stringr::regex("latitude", ignore_case = TRUE)) ~ "Latitude",
          stringr::str_detect(.data[[data_name_col]], stringr::regex("longitude", ignore_case = TRUE)) ~ "Longitude"
        )
      )
  }

  loc_data <- loc_data |>
    dplyr::filter(!is.na(.data$value)) |>
    dplyr::filter(!is.na(.data$coord_type)) |>
    # Keep only values that look like numbers (digits, optional sign, decimal point)
    # This removes place names, empty strings, and other non-numeric text
    # so as.numeric() below won't produce coercion warnings
    dplyr::filter(grepl("^[+-]?[0-9]*\\.?[0-9]+$", .data$value, useBytes = TRUE)) |>
    dplyr::mutate(value = as.numeric(.data$value))

  # Apply manual corrections if provided
  if (!is.null(corrections)) {
    loc_data <- .apply_corrections(loc_data, corrections, observation_col, dataset_col)
  }

  # Average when multiple values exist per observation
  loc_data <- loc_data |>
    dplyr::group_by(
      .data[[dataset_col]], .data$Dataset, .data[[observation_col]], .data$coord_type
    ) |>
    dplyr::summarize(value = mean(.data$value, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = "coord_type", values_from = "value") |>
    dplyr::filter(!is.na(.data$Latitude) & !is.na(.data$Longitude))

  cli::cli_inform("Extracted coordinates for {nrow(loc_data)} observation{?s}.")
  loc_data
}


# Internal: apply user corrections to location data
.apply_corrections <- function(loc_data, corrections, observation_col, dataset_col) {
  if ("Latitude" %in% names(corrections) && observation_col %in% names(corrections)) {
    lat_corr <- corrections |>
      dplyr::filter(!is.na(.data$Latitude)) |>
      dplyr::select(dplyr::all_of(c(observation_col, dataset_col)), .data$Latitude)

    for (i in seq_len(nrow(lat_corr))) {
      mask <- loc_data[[observation_col]] == lat_corr[[observation_col]][i] &
        loc_data$coord_type == "Latitude"
      if (!is.null(lat_corr[[dataset_col]])) {
        mask <- mask & loc_data[[dataset_col]] == lat_corr[[dataset_col]][i]
      }
      loc_data$value[mask] <- lat_corr$Latitude[i]
    }
  }

  if ("Longitude" %in% names(corrections) && observation_col %in% names(corrections)) {
    lon_corr <- corrections |>
      dplyr::filter(!is.na(.data$Longitude)) |>
      dplyr::select(dplyr::all_of(c(observation_col, dataset_col)), .data$Longitude)

    for (i in seq_len(nrow(lon_corr))) {
      mask <- loc_data[[observation_col]] == lon_corr[[observation_col]][i] &
        loc_data$coord_type == "Longitude"
      if (!is.null(lon_corr[[dataset_col]])) {
        mask <- mask & loc_data[[dataset_col]] == lon_corr[[dataset_col]][i]
      }
      loc_data$value[mask] <- lon_corr$Longitude[i]
    }
  }

  loc_data
}


#' Extract climate zones from coordinates
#'
#' Assigns Koppen-Geiger climate zone classifications to coordinates. The
#' package ships a 0.1-degree resolution raster and legend from Beck et al.
#' (2023), so no external files are needed. You can also supply your own
#' raster and legend. Requires the `terra` and `sf` packages.
#'
#' @param coords A data frame with `Latitude` and `Longitude` columns
#'   (as returned by [extract_coordinates()]).
#' @param climate_raster A `SpatRaster` object (from `terra::rast()`).
#'   If `NULL` (default), uses the bundled Koppen-Geiger raster.
#' @param legend A data frame mapping raster values to climate zone names.
#'   If `NULL` (default) and using the bundled raster, the bundled legend
#'   is loaded automatically.
#'
#' @return The input data frame with added `Climate` (integer raster value),
#'   `climate_code` (e.g., "Cfb"), and `climate_description` columns.
#' @export
extract_climate_zones <- function(coords, climate_raster = NULL, legend = NULL) {
  rlang::check_installed("terra", reason = "to extract climate zones from raster data")
  rlang::check_installed("sf", reason = "to work with spatial coordinates")

  if (nrow(coords) == 0) {
    cli::cli_warn("No coordinates provided.")
    coords$Climate <- integer()
    coords$climate_code <- character()
    coords$climate_description <- character()
    return(coords)
  }

  # Use bundled Koppen-Geiger data if no raster provided
  if (is.null(climate_raster)) {
    raster_path <- system.file("extdata", "koppen_geiger_0p1.tif",
                               package = "tidyTRY")
    if (raster_path == "") {
      cli::cli_abort("Bundled climate raster not found. Reinstall tidyTRY.")
    }
    climate_raster <- terra::rast(raster_path)
  }

  if (is.null(legend)) {
    legend <- .load_climate_legend()
  }

  coords_sf <- sf::st_as_sf(
    coords,
    coords = c("Longitude", "Latitude"),
    crs = 4326,
    agr = "constant"
  )

  coords$Climate <- terra::extract(climate_raster, coords_sf)[, 2]

  if (!is.null(legend)) {
    coords <- dplyr::left_join(coords, legend, by = "Climate")
  }

  coords <- coords |>
    dplyr::filter(!is.na(.data$Climate))

  cli::cli_inform("Assigned climate zones to {nrow(coords)} observation{?s}.")
  coords
}


# Internal: load the bundled Koppen-Geiger legend
.load_climate_legend <- function() {
  legend_path <- system.file("extdata", "koppen_geiger_legend.txt",
                             package = "tidyTRY")
  if (legend_path == "") return(NULL)

  lines <- readLines(legend_path)
  # Keep only lines that start with a number (the actual legend entries)
  data_lines <- lines[grepl("^\\s*\\d+:", lines)]

  # Parse: "    1:  Af   Tropical, rainforest                  [0 0 255]"
  legend <- do.call(rbind, lapply(data_lines, function(line) {
    # Remove the RGB bracket part
    line <- sub("\\[.*\\]", "", line)
    # Extract: number, code, description
    m <- regmatches(line, regexec("^\\s*(\\d+):\\s+(\\S+)\\s+(.+)$", line))[[1]]
    data.frame(
      Climate = as.integer(m[2]),
      climate_code = trimws(m[3]),
      climate_description = trimws(m[4]),
      stringsAsFactors = FALSE
    )
  }))

  legend
}
