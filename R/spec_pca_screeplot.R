#' Create an elegant scree plot for Principal Component Analysis (PCA)
#'
#' @description
#' Generates a scree plot from a `prcomp` object, showing both the variance explained
#' by each principal component and the cumulative variance.
#'
#' @param .data Result object from `prcomp()` function
#' @param N Maximum number of principal components to display (default: 10)
#'
#' @return A ggplot object containing the scree plot
#'
#' @details
#' The plot includes:
#' - Blue bars representing the proportion of variance explained by each component
#' - Red line showing the cumulative variance
#' - Green dashed line representing the Kaiser criterion (variance > 1)
#' - Variance percentages above each bar
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' pca_result <- prcomp(iris[,1:4], scale. = TRUE)
#' spec_pca_screeplot(pca_result)
#' spec_pca_screeplot(pca_result, N = 4)  # Showing only 4 components
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_point geom_hline
#' @importFrom ggplot2 scale_y_continuous sec_axis labs theme_minimal geom_text
#' @importFrom scales percent
#' @export
spec_pca_screeplot <- function(.data, N = 10) {
  # Verify input is a prcomp object
  if (!inherits(.data, "prcomp")) {
    stop("The .data argument must be an object returned by prcomp()")
  }

  # Extract variances
  variances <- .data$sdev^2

  # Ensure N doesn't exceed available components
  N <- min(N, length(variances))

  # Calculate variance proportions
  prop_var <- variances / sum(variances)
  cum_var <- cumsum(prop_var)

  # Create plot data frame
  df_plot <- data.frame(
    Component = factor(1:length(variances)),
    Variance = prop_var,
    Cumulative = cum_var
  )[1:N, ]

  # Kaiser criterion (variance > 1)
  kaiser <- 1/length(variances)

  # Create the plot
  ggplot2::ggplot(df_plot, ggplot2::aes(x = Component, y = Variance)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = Cumulative, group = 1),
                       color = "red", linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(y = Cumulative),
                        color = "red", size = 3) +
    ggplot2::geom_hline(yintercept = kaiser,
                        linetype = "dashed", color = "darkgreen") +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(Variance, accuracy = 0.1)),
                       vjust = -0.5, size = 3.5) +
    ggplot2::scale_y_continuous(
      name = "Proportion of Explained Variance",
      labels = scales::percent,
      sec.axis = ggplot2::sec_axis(~.,
                                   name = "Cumulative Variance",
                                   labels = scales::percent)
    ) +
    ggplot2::labs(
      title = "Scree Plot - Principal Component Analysis",
      subtitle = paste("Kaiser criterion (green line):",
                       round(kaiser*100, 1), "%"),
      x = "Principal Components",
      caption = "Blue bars: Individual variance\nRed line: Cumulative variance"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      axis.title.y.right = ggplot2::element_text(color = "red"),
      axis.text.y.right = ggplot2::element_text(color = "red")
    )
}
