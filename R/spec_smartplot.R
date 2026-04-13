#' Create a Custom Plot for Spectral Data
#'
#' This function generates a customizable plot for spectral data, allowing for
#' the selection of plot type (absorbance or transmittance), x-axis direction,
#' plot geometry (points or lines), and color palette.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength
#'   or wavenumber data. This parameter is required.
#' @param xdir A character string specifying the direction of the x-axis.
#'   Choices are `"reverse"` (typically used for wavenumber) or `"standard"`.
#' @param geom A character string specifying the geometry of the plot.
#'   Choices are `"point"` for a scatter plot or `"line"` for a line plot.
#' @param xmin A numeric value specifying the minimum x-axis value.
#'   If not provided, the minimum value from `wn_col` will be used.
#' @param xmax A numeric value specifying the maximum x-axis value.
#'   If not provided, the maximum value from `wn_col` will be used.
#' @param alpha A numeric value (0–1) specifying the transparency of plotted
#'   points or lines. Default is `0.8`.
#' @param type A character string specifying the y-axis label.
#'   Either `"absorbance"` (default) or `"transmittance"`.
#' @param palette A character string specifying the color palette to use.
#'   Built-in options:
#'   \describe{
#'     \item{`"viridis"`}{Perceptually uniform, colorblind-friendly (default).}
#'     \item{`"plasma"`}{High-contrast warm-to-cool gradient.}
#'     \item{`"magma"`}{Dark-to-light, suitable for dark backgrounds.}
#'     \item{`"cividis"`}{Optimized for color vision deficiency.}
#'     \item{`"turbo"`}{Full-spectrum rainbow with high contrast.}
#'     \item{`"Set1"`}{Qualitative palette from RColorBrewer (up to 9 colors).}
#'     \item{`"Set2"`}{Softer qualitative palette (up to 8 colors).}
#'     \item{`"Dark2"`}{Darker qualitative palette (up to 8 colors).}
#'     \item{`"Paired"`}{Paired qualitative palette (up to 12 colors).}
#'     \item{`"custom"`}{Use a custom vector of colors via `custom_colors`.}
#'   }
#' @param custom_colors A character vector of valid color strings (hex codes or
#'   R color names) used when `palette = "custom"`. The number of colors should
#'   match or exceed the number of spectra in the data. If fewer colors are
#'   provided than spectra, colors will be recycled with a warning.
#'
#' @return A `ggplot` object representing the spectral plot.
#'
#' @importFrom ggplot2 ggplot aes scale_x_continuous scale_x_reverse
#'   scale_color_viridis_d scale_color_manual xlab ylab theme
#'   element_text element_rect element_line geom_line geom_point labs
#' @importFrom dplyr all_of filter %>%
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \dontrun{
#' # Default viridis palette
#' spec_smartplot(spec_data, wn_col = "wavenumber")
#'
#' # Built-in qualitative palette
#' spec_smartplot(spec_data, wn_col = "wavenumber", palette = "Set1")
#'
#' # Custom colors
#' spec_smartplot(spec_data, wn_col = "wavenumber",
#'                palette = "custom",
#'                custom_colors = c("#E63946", "#457B9D", "#2A9D8F"))
#' }
#'
#' @export
spec_smartplot <- function(.data,
                           wn_col        = NULL,
                           xdir          = c("reverse", "standard"),
                           geom          = c("point", "line"),
                           xmin          = NULL,
                           xmax          = NULL,
                           alpha         = 0.8,
                           type          = c("absorbance", "transmittance"),
                           palette       = c("viridis", "plasma", "magma",
                                             "cividis", "turbo",
                                             "Set1", "Set2", "Dark2", "Paired",
                                             "custom"),
                           custom_colors = NULL) {

  # ── Input validation ────────────────────────────────────────────────────────

  if (!inherits(.data, "data.frame")) {
    stop("The argument .data must be a data.frame or tibble.")
  }

  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env, ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("wn_col not specified and no default defined with set_spec_wn().")
    } else {
      warn_missing_param_once("wn_col", wn_col)
    }
  }

  if (!(wn_col %in% names(.data))) {
    stop(sprintf("Column '%s' was not found in the dataset.", wn_col))
  }

  wn_values <- .data[[wn_col]]

  if (!is.numeric(wn_values)) {
    stop(sprintf("Column '%s' must be numeric for plotting.", wn_col))
  }

  if (is.null(xmin)) {
    xmin <- min(wn_values, na.rm = TRUE)
    warn_missing_param_once("xmin", xmin)
  } else if (!is.numeric(xmin)) {
    warning("xmin must be numeric. Using the minimum value from the data.")
    xmin <- min(wn_values, na.rm = TRUE)
  }

  if (is.null(xmax)) {
    xmax <- max(wn_values, na.rm = TRUE)
    warn_missing_param_once("xmax", xmax)
  } else if (!is.numeric(xmax)) {
    warning("xmax must be numeric. Using the maximum value from the data.")
    xmax <- max(wn_values, na.rm = TRUE)
  }

  if (xmin >= xmax) {
    stop("xmin must be less than xmax.")
  }

  if (!is.numeric(alpha) || alpha < 0 || alpha > 1) {
    warning("alpha must be numeric between 0 and 1. Using default value 0.8.")
    alpha <- 0.8
  }

  xdir    <- match.arg(xdir)
  geom    <- match.arg(geom)
  type    <- match.arg(type)
  palette <- match.arg(palette)

  # ── Palette validation ───────────────────────────────────────────────────────

  brewer_palettes <- c("Set1", "Set2", "Dark2", "Paired")
  brewer_max      <- c(Set1 = 9L, Set2 = 8L, Dark2 = 8L, Paired = 12L)

  if (palette == "custom") {
    if (is.null(custom_colors)) {
      stop(
        "palette = 'custom' requires a character vector supplied to custom_colors."
      )
    }
    if (!is.character(custom_colors)) {
      stop("custom_colors must be a character vector of valid color strings.")
    }
    # Validate that all strings are recognizable colors
    invalid <- custom_colors[
      !vapply(custom_colors, function(col) {
        tryCatch({ grDevices::col2rgb(col); TRUE }, error = function(e) FALSE)
      }, logical(1))
    ]
    if (length(invalid) > 0) {
      stop(sprintf(
        "The following values in custom_colors are not valid colors: %s",
        paste(invalid, collapse = ", ")
      ))
    }
  }

  # ── Data preparation ─────────────────────────────────────────────────────────

  plot_data <- .data %>%
    tidyr::pivot_longer(
      cols      = -dplyr::all_of(wn_col),
      names_to  = "spectra",
      values_to = "vals"
    ) %>%
    dplyr::filter(.data[[wn_col]] >= xmin, .data[[wn_col]] <= xmax)

  if (nrow(plot_data) == 0) {
    warning(
      "No data available after applying xmin and xmax limits. ",
      "Check the specified values."
    )
  }

  n_spectra <- length(unique(plot_data[["spectra"]]))

  # Warn if a RColorBrewer palette exceeds its maximum
  if (palette %in% brewer_palettes) {
    max_cols <- brewer_max[[palette]]
    if (n_spectra > max_cols) {
      warning(sprintf(
        "Palette '%s' supports at most %d colors, but %d spectra were found. ",
        palette, max_cols, n_spectra,
        "Consider using a continuous palette (e.g. 'viridis') or 'custom'."
      ))
    }
  }

  # Recycle custom colors if needed
  if (palette == "custom" && length(custom_colors) < n_spectra) {
    warning(sprintf(
      "custom_colors has %d color(s) but %d spectra were found. ",
      length(custom_colors), n_spectra,
      "Colors will be recycled."
    ))
    custom_colors <- rep_len(custom_colors, n_spectra)
  }

  # ── Base plot ────────────────────────────────────────────────────────────────

  viridis_options <- c("viridis" = "D", "plasma" = "C",
                       "magma"   = "A", "cividis" = "E", "turbo" = "H")

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data[[wn_col]], y = vals, col = spectra)
  ) +
    ggplot2::xlab(expression(Wavenumber ~ (cm^-1))) +
    ggplot2::ylab(type) +
    ggplot2::labs(col = "Spectra") +
    ggplot2::theme(
      text             = ggplot2::element_text(family = "serif"),
      panel.background = ggplot2::element_rect(fill = "white",
                                               linetype = "solid"),
      panel.grid.major = ggplot2::element_line(colour    = "black",
                                               linewidth = 0.05),
      panel.grid.minor = ggplot2::element_line(colour    = "grey50",
                                               linewidth = 0.01),
      panel.border     = ggplot2::element_rect(linetype  = "solid",
                                               fill      = NA,
                                               linewidth = 1)
    )

  # ── Color scale ──────────────────────────────────────────────────────────────

  if (palette %in% names(viridis_options)) {
    p <- p + ggplot2::scale_color_viridis_d(option = viridis_options[[palette]])

  } else if (palette %in% brewer_palettes) {
    p <- p + ggplot2::scale_color_brewer(palette = palette, type = "qual")

  } else if (palette == "custom") {
    p <- p + ggplot2::scale_color_manual(values = custom_colors)
  }

  # ── x-axis direction ─────────────────────────────────────────────────────────

  if (xdir == "reverse") {
    p <- p + ggplot2::scale_x_reverse(limits = c(xmax, xmin))
  } else {
    p <- p + ggplot2::scale_x_continuous(limits = c(xmin, xmax))
  }

  # ── Geometry ─────────────────────────────────────────────────────────────────

  if (geom == "line") {
    p <- p + ggplot2::geom_line(alpha = alpha, linewidth = 1.25)
  } else {
    p <- p + ggplot2::geom_point(alpha = alpha)
  }

  return(p)
}
