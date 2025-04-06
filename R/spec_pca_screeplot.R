#' Scree plot for PCA results
#'
#' Creates a customizable scree plot based on a `prcomp` object.
#'
#' @param pca A PCA object returned by [prcomp()].
#' @param n Number of components to display. Defaults to 10.
#' @param show_labels Logical. Show percentage labels on bars? Default is TRUE.
#' @param show_cumulative Logical. Show cumulative variance line? Default is TRUE.
#' @param bar_color Fill color for bars. Default is "steelblue".
#' @param line_color Color of the cumulative line and points. Default is "darkred".
#' @param show_kaiser Logical. Show Kaiser criterion line? Default is FALSE.
#'
#' @return A ggplot2 scree plot.
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_point geom_text
#' @importFrom ggplot2 scale_y_continuous labs theme_minimal theme element_text
#' @importFrom ggplot2 annotate
#' @importFrom scales percent
#' @importFrom stats prcomp
#' @export
#'
#' @examples
#' pca <- prcomp(USArrests, scale. = TRUE)
#' spec_pca_screeplot(pca, n = 5)
spec_pca_screeplot <- function(pca,
                               n = 10,
                               show_labels = TRUE,
                               show_cumulative = TRUE,
                               bar_color = "steelblue",
                               line_color = "darkred",
                               show_kaiser = FALSE) {

  if (!inherits(pca, "prcomp")) {
    stop("The input must be a 'prcomp' object.")
  }

  variances <- pca$sdev^2
  n <- min(n, length(variances))
  prop_var <- variances / sum(variances)
  cum_var <- cumsum(prop_var)

  df_plot <- data.frame(
    Component = factor(1:length(variances)),
    Variance = prop_var,
    Cumulative = cum_var
  )[1:n, ]

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = Component, y = Variance)) +
    ggplot2::geom_bar(stat = "identity", fill = bar_color, alpha = 0.85)

  if (show_cumulative) {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(y = Cumulative, group = 1),
        color = line_color, linewidth = 1
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = Cumulative),
        color = line_color, size = 2
      )
  }

  if (show_labels) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = scales::percent(Variance, accuracy = 0.1)),
        vjust = -0.5, size = 3.2
      )
  }

  if (show_kaiser) {
    kaiser <- 1 / length(variances)
    p <- p +
      ggplot2::geom_hline(yintercept = kaiser,
                          linetype = "dashed", color = "darkgreen") +
      ggplot2::annotate("text", x = 1, y = kaiser,
                        label = "Kaiser criterion", vjust = -1.2,
                        color = "darkgreen", size = 3)
  }

  p +
    ggplot2::scale_y_continuous(
      name = "Explained Variance",
      labels = scales::percent
    ) +
    ggplot2::labs(
      title = "Scree Plot",
      x = "Principal Components"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
    )
}
