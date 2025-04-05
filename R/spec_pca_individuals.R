#' Create a PCA Individuals (Scores) Plot
#'
#' @description
#' Generates a 2D plot of individual observations based on their PCA scores.
#'
#' @param .data Result object from `prcomp()` function
#' @param pc_x Principal component to plot on x-axis (default: 1)
#' @param pc_y Principal component to plot on y-axis (default: 2)
#' @param color_var Optional variable to color points by (length must match input data)
#' @param label_points Logical, whether to label points (default: FALSE)
#' @param point_size Size of points (default: 3)
#' @param alpha Point transparency (default: 0.7)
#'
#' @return A ggplot object showing the PCA scores plot
#'
#' @details
#' The plot displays:
#' - Each observation as a point in PC space
#' - Option to color by a grouping variable
#' - Percentage of explained variance for each axis
#'
#' @examples
#' \dontrun{
#' # Basic usage:
#' pca_result <- prcomp(iris[,1:4], scale. = TRUE)
#' spec_pca_individuals(pca_result)
#'
#' # With coloring by species:
#' spec_pca_individuals(pca_result, color_var = iris$Species)
#'
#' # Customizing components:
#' spec_pca_individuals(pca_result, pc_x = 1, pc_y = 3)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_text labs theme_minimal
#' @importFrom ggrepel geom_text_repel
#' @export
spec_pca_individuals <- function(.data, pc_x = 1, pc_y = 2,
                                 color_var = NULL, label_points = FALSE,
                                 point_size = 3, alpha = 0.7) {

  # Verify input is a prcomp object
  if (!inherits(.data, "prcomp")) {
    stop("The .data argument must be an object returned by prcomp()")
  }

  # Extract scores
  scores <- as.data.frame(.data$x)

  # Check requested PCs exist
  max_pc <- ncol(scores)
  if (pc_x > max_pc || pc_y > max_pc) {
    stop(paste("Requested component exceeds available components (max =", max_pc, ")"))
  }

  # Prepare data frame for plotting
  plot_data <- data.frame(
    x = scores[, pc_x],
    y = scores[, pc_y],
    label = rownames(scores)
  )

  # Add color variable if provided
  if (!is.null(color_var)) {
    if (length(color_var) != nrow(scores)) {
      stop("color_var length must match number of observations in PCA input")
    }
    plot_data$color_var <- color_var
  }

  # Calculate variance explained
  variances <- .data$sdev^2
  prop_var <- round(variances / sum(variances) * 100, 1)

  # Create base plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::labs(
      x = paste0("PC", pc_x, " (", prop_var[pc_x], "%)"),
      y = paste0("PC", pc_y, " (", prop_var[pc_y], "%)"),
      title = "PCA Individuals Plot"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      legend.position = "bottom"
    )

  # Add points with or without color
  if (!is.null(color_var)) {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(color = color_var),
                          size = point_size, alpha = alpha) +
      ggplot2::labs(color = "")
  } else {
    p <- p +
      ggplot2::geom_point(size = point_size, alpha = alpha, color = "steelblue")
  }

  # Add labels if requested
  if (label_points) {
    if (!is.null(color_var)) {
      p <- p + ggrepel::geom_text_repel(
        ggplot2::aes(label = label, color = color_var),
        show.legend = FALSE
      )
    } else {
      p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = label))
    }
  }

  # Add center lines
  p <- p +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)

  return(p)
}
