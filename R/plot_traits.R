#' Plot trait distributions
#'
#' Generates density and boxplot panels for each species and trait combination.
#' Useful for visual inspection and outlier detection.
#'
#' @param data A data frame of cleaned TRY data.
#' @param species_col Name of the species column. Defaults to `"AccSpeciesName"`.
#' @param trait_col Name of the trait column. Defaults to `"TraitName"`.
#' @param value_col Name of the numeric value column. Defaults to `"StdValue"`.
#' @param output_file Optional file path for PDF output. If `NULL` (default),
#'   returns a list of ggplot objects.
#' @param width PDF page width in inches. Defaults to 10.
#' @param height PDF page height in inches. Defaults to 8.
#'
#' @return If `output_file` is `NULL`, a named list of ggplot objects (one per
#'   species). If `output_file` is provided, writes a PDF and returns the path
#'   invisibly.
#'
#' @export
plot_trait_distributions <- function(data,
                                     species_col = "AccSpeciesName",
                                     trait_col = "TraitName",
                                     value_col = "StdValue",
                                     output_file = NULL,
                                     width = 10,
                                     height = 8) {
  rlang::check_installed("ggplot2", reason = "to create diagnostic plots")
  rlang::check_installed("patchwork", reason = "to combine density and boxplot panels")

  species_list <- unique(data[[species_col]])
  species_list <- species_list[!is.na(species_list) & species_list != ""]

  if (length(species_list) == 0) {
    cli::cli_warn("No species found in the data.")
    return(invisible(list()))
  }

  plots <- list()

  for (sp in species_list) {
    sp_data <- data |>
      dplyr::filter(
        .data[[species_col]] == sp &
          !is.na(.data[[value_col]])
      )

    if (nrow(sp_data) == 0) next

    p1 <- ggplot2::ggplot(sp_data, ggplot2::aes(x = .data[[value_col]])) +
      ggplot2::theme_bw() +
      ggplot2::geom_density() +
      ggplot2::facet_wrap(
        stats::as.formula(paste("~", trait_col)),
        scales = "free", ncol = 3, drop = TRUE
      ) +
      ggplot2::labs(title = sp)

    p2 <- ggplot2::ggplot(
      sp_data,
      ggplot2::aes(x = .data[[trait_col]], y = .data[[value_col]])
    ) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot() +
      ggplot2::geom_jitter(size = 0.5) +
      ggplot2::facet_wrap(
        stats::as.formula(paste("~", trait_col)),
        scales = "free", ncol = 3, drop = TRUE
      )

    plots[[sp]] <- p1 + p2
  }

  if (!is.null(output_file)) {
    grDevices::pdf(file = output_file, width = width, height = height)
    on.exit(grDevices::dev.off(), add = TRUE)

    for (p in plots) {
      print(p)
    }

    cli::cli_inform("Saved trait distribution plots to {.path {output_file}}")
    return(invisible(output_file))
  }

  cli::cli_inform("Generated plots for {length(plots)} species.")
  plots
}
