#' Create an Interactive Plot for Spectral Data (plotly)
#'
#' This function generates a customizable interactive plot for spectral data
#' using plotly, allowing for the selection of plot type (absorbance or
#' transmittance), x-axis direction, plot geometry (points or lines), and
#' color palette.
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
#' @return A `plotly` object representing the interactive spectral plot.
#'
#' @importFrom plotly plot_ly add_lines add_markers layout config
#' @importFrom dplyr all_of filter %>%
#' @importFrom tidyr pivot_longer
#' @importFrom grDevices col2rgb
#' @importFrom viridisLite viridis
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
#' \dontrun{
#' # Default viridis palette, line geometry
#' spec_smartplotly(spec_data, wn_col = "wavenumber", geom = "line")
#'
#' # Built-in qualitative palette
#' spec_smartplotly(spec_data, wn_col = "wavenumber", palette = "Set1")
#'
#' # Custom colors
#' spec_smartplotly(spec_data, wn_col = "wavenumber",
#'                       palette = "custom",
#'                       custom_colors = c("#E63946", "#457B9D", "#2A9D8F"))
#' }
#'
#' @export
spec_smartplotly <- function(.data,
                                  wn_col        = NULL,
                                  xdir          = c("reverse", "standard"),
                                  geom          = c("point", "line"),
                                  xmin          = NULL,
                                  xmax          = NULL,
                                  alpha         = 0.8,
                                  type          = c("absorbance", "transmittance"),
                                  palette       = c("viridis", "plasma", "magma",
                                                    "cividis", "turbo",
                                                    "Set1", "Set2", "Dark2",
                                                    "Paired", "custom"),
                                  custom_colors = NULL) {

  # ── Input validation ─────────────────────────────────────────────────────────

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

  spectra_names <- unique(plot_data[["spectra"]])
  n_spectra     <- length(spectra_names)

  # ── Resolve color vector ─────────────────────────────────────────────────────

  .resolve_colors <- function(palette, n, custom_colors,
                              brewer_palettes, brewer_max) {
    viridis_options <- c(
      "viridis" = "D", "plasma" = "C",
      "magma"   = "A", "cividis" = "E", "turbo" = "H"
    )

    if (palette %in% names(viridis_options)) {
      cols <- viridisLite::viridis(
        n      = n,
        option = viridis_options[[palette]],
        begin  = 0.05,
        end    = 0.95
      )

    } else if (palette %in% brewer_palettes) {
      max_cols <- brewer_max[[palette]]
      if (n > max_cols) {
        warning(sprintf(
          paste0("Palette '%s' supports at most %d colors, but %d spectra ",
                 "were found. Consider using a continuous palette ",
                 "(e.g. 'viridis') or 'custom'."),
          palette, max_cols, n
        ))
      }
      cols <- RColorBrewer::brewer.pal(n = min(n, max_cols), name = palette)
      if (n > max_cols) cols <- rep_len(cols, n)

    } else if (palette == "custom") {
      if (length(custom_colors) < n) {
        warning(sprintf(
          "custom_colors has %d color(s) but %d spectra were found. Colors will be recycled.",
          length(custom_colors), n
        ))
        custom_colors <- rep_len(custom_colors, n)
      }
      cols <- custom_colors[seq_len(n)]
    }

    # Convert any R color name to hex so plotly always gets valid hex strings
    vapply(cols, function(col) {
      rgb_vals <- grDevices::col2rgb(col)
      grDevices::rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], maxColorValue = 255)
    }, character(1), USE.NAMES = FALSE)
  }

  color_vector <- .resolve_colors(
    palette         = palette,
    n               = n_spectra,
    custom_colors   = custom_colors,
    brewer_palettes = brewer_palettes,
    brewer_max      = brewer_max
  )

  names(color_vector) <- spectra_names

  # ── x-axis range & direction ─────────────────────────────────────────────────

  x_range <- if (xdir == "reverse") c(xmax, xmin) else c(xmin, xmax)

  # ── Build plotly figure ───────────────────────────────────────────────────────

  p <- plotly::plot_ly()

  for (i in seq_along(spectra_names)) {
    sp    <- spectra_names[[i]]
    color <- color_vector[[sp]]
    df    <- plot_data[plot_data[["spectra"]] == sp, ]

    if (geom == "line") {
      p <- plotly::add_lines(
        p,
        x          = df[[wn_col]],
        y          = df[["vals"]],
        name       = sp,
        opacity    = alpha,
        line       = list(color = color, width = 2),
        showlegend = TRUE
      )
    } else {
      p <- plotly::add_markers(
        p,
        x          = df[[wn_col]],
        y          = df[["vals"]],
        name       = sp,
        opacity    = alpha,
        marker     = list(color = color, size = 4),
        showlegend = TRUE
      )
    }
  }

  # ── Layout ───────────────────────────────────────────────────────────────────

  y_label <- if (type == "absorbance") "Absorbance" else "Transmittance"

  p <- plotly::layout(
    p,
    xaxis = list(
      title     = "Wavenumber (cm\u207b\u00b9)",
      range     = x_range,
      showgrid  = TRUE,
      gridcolor = "rgba(0,0,0,0.08)",
      linecolor = "black",
      linewidth = 1,
      mirror    = TRUE,
      ticks     = "outside"
    ),
    yaxis = list(
      title     = y_label,
      showgrid  = TRUE,
      gridcolor = "rgba(0,0,0,0.08)",
      linecolor = "black",
      linewidth = 1,
      mirror    = TRUE,
      ticks     = "outside"
    ),
    legend = list(
      title = list(text = "<b>Spectra</b>")
    ),
    plot_bgcolor  = "white",
    paper_bgcolor = "white",
    font          = list(family = "serif")
  )

  p <- plotly::config(
    p,
    displaylogo      = FALSE,
    modeBarButtonsToRemove = c("select2d", "lasso2d")
  )

  return(p)
}
