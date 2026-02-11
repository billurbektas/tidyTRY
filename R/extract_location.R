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
    # Remove values that contain letters (place names, not coordinates)
    dplyr::filter(!stringr::str_detect(.data$value, "[a-zA-Z]"))

  # Convert to numeric, muffling "NAs introduced by coercion" --
  # non-numeric values become NA and are filtered on the next line
  loc_data <- withCallingHandlers(
    loc_data |> dplyr::mutate(value = as.numeric(.data$value)),
    warning = function(w) {
      if (grepl("NAs introduced by coercion", conditionMessage(w)))
        invokeRestart("muffleWarning")
    }
  ) |>
    dplyr::filter(!is.na(.data$value))

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
#' Assigns climate zone classifications to coordinates using a raster map
#' (e.g., Koppen-Geiger). Requires the `terra` and `sf` packages.
#'
#' @param coords A data frame with `Latitude` and `Longitude` columns
#'   (as returned by [extract_coordinates()]).
#' @param climate_raster A `SpatRaster` object (from `terra::rast()`)
#'   containing climate zone classifications.
#' @param legend Optional data frame mapping raster values to climate
#'   zone names. Should have a column matching the raster values and
#'   descriptive columns.
#'
#' @return The input data frame with an added `Climate` column (and any
#'   legend columns if provided).
#' @export
extract_climate_zones <- function(coords, climate_raster, legend = NULL) {
  rlang::check_installed("terra", reason = "to extract climate zones from raster data")
  rlang::check_installed("sf", reason = "to work with spatial coordinates")

  if (nrow(coords) == 0) {
    cli::cli_warn("No coordinates provided.")
    coords$Climate <- integer()
    return(coords)
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
