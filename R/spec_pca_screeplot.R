#' Scree plot for PCA results
#'
#' Creates a customizable scree plot based on a `prcomp` object showing variance explained by each component.
#'
#' @param pca A PCA object returned by [prcomp()].
#' @param n Number of components to display. Defaults to 10.
#' @param show_labels Logical. Show percentage labels on bars? Default is TRUE.
#' @param show_cumulative Logical. Show cumulative variance line? Default is TRUE.
#' @param bar_color Fill color for bars. Default is "steelblue".
#' @param line_color Color of the cumulative line and points. Default is "darkred".
#' @param show_kaiser Logical. Show Kaiser criterion line? Default is FALSE.
#' @param title Plot title. Default is "Scree Plot".
#' @param subtitle Optional plot subtitle.
#' @param accuracy Number of decimal places for variance percentages. Default is 1.
#'
#' @return A ggplot2 scree plot object.
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_point geom_text
#' @importFrom ggplot2 scale_y_continuous labs theme_minimal theme element_text
#' @importFrom ggplot2 annotate geom_hline waiver
#' @importFrom scales percent
#' @importFrom glue glue
#' @export
#'
#' @examples
#' pca <- prcomp(USArrests, scale. = TRUE)
#' spec_pca_screeplot(pca, n = 4)
#' spec_pca_screeplot(pca, show_kaiser = TRUE, bar_color = "darkblue")
spec_pca_screeplot <- function(pca,
                               n = 10,
                               show_labels = TRUE,
                               show_cumulative = TRUE,
                               bar_color = "steelblue",
                               line_color = "darkred",
                               show_kaiser = FALSE,
                               title = "Scree Plot",
                               subtitle = NULL,
                               accuracy = 1) {

  # Input validation
  if (!inherits(pca, "prcomp")) {
    stop("Input must be a prcomp object", call. = FALSE)
  }

  variances <- pca$sdev^2
  total_components <- length(variances)

  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != floor(n)) {
    stop("'n' must be a positive integer", call. = FALSE)
  }
  n <- min(n, total_components)

  if (!is.logical(show_labels) || length(show_labels) != 1) {
    stop("'show_labels' must be a single logical value", call. = FALSE)
  }

  if (!is.logical(show_cumulative) || length(show_cumulative) != 1) {
    stop("'show_cumulative' must be a single logical value", call. = FALSE)
  }

  if (!is.logical(show_kaiser) || length(show_kaiser) != 1) {
    stop("'show_kaiser' must be a single logical value", call. = FALSE)
  }

  if (!is.character(bar_color) || length(bar_color) != 1) {
    warning("'bar_color' must be a single color value, using default")
    bar_color <- "steelblue"
  }

  if (!is.character(line_color) || length(line_color) != 1) {
    warning("'line_color' must be a single color value, using default")
    line_color <- "darkred"
  }

  if (!is.numeric(accuracy) || length(accuracy) != 1 || accuracy < 0) {
    warning("'accuracy' must be a positive number, using default (1)")
    accuracy <- 1
  }

  # Prepare data
  prop_var <- variances / sum(variances)
  cum_var <- cumsum(prop_var)

  df_plot <- data.frame(
    Component = factor(1:total_components, levels = 1:total_components),
    Variance = prop_var,
    Cumulative = cum_var
  )[1:n, ]

  # Base plot
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = Component, y = Variance)) +
    ggplot2::geom_bar(stat = "identity", fill = bar_color, alpha = 0.85, width = 0.7) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      sec.axis = if (show_cumulative) {
        ggplot2::sec_axis(
          ~ .,
          name = "Cumulative Variance",
          labels = scales::percent
        )
      } else {
        waiver()
      }
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Principal Component",
      y = "Variance Explained"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "grey50")
    )

  # Add cumulative line if requested
  if (show_cumulative) {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(y = Cumulative, group = 1),
        color = line_color, linewidth = 1
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = Cumulative),
        color = line_color, size = 2.5
      )
  }

  # Add variance labels if requested
  if (show_labels) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = scales::percent(Variance, accuracy = 0.1 * accuracy)),
        vjust = -0.5,
        size = 3.2
      )
  }

  # Add Kaiser criterion if requested
  if (show_kaiser) {
    kaiser <- 1 / length(variances)
    p <- p +
      ggplot2::geom_hline(
        yintercept = kaiser,
        linetype = "dashed",
        color = "darkgreen",
        linewidth = 0.7
      ) +
      ggplot2::annotate(
        "text",
        x = Inf, y = kaiser,
        label = glue::glue("Kaiser criterion ({scales::percent(kaiser, accuracy = 0.1)})"),
        hjust = 1.1,
        vjust = -0.5,
        color = "darkgreen",
        size = 3
      )
  }

  return(p)
}
