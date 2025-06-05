#' Rolling Ball Baseline Correction
#'
#' Implements the rolling ball baseline correction method
#'
#' @importFrom graphics lines legend grid par
#' @importFrom stats rnorm
#'
#' @param x Numeric vector containing the spectrum/signal values
#' @param wm Window width (ball radius). Larger values = smoother baseline
#' @param ws Smoothing window width (optional)
#' @return A list with three components:
#'   \describe{
#'     \item{baseline}{Numeric vector containing the estimated baseline values}
#'     \item{corrected}{Numeric vector containing the baseline-corrected signal (original - baseline)}
#'     \item{original}{Numeric vector containing the original input signal}
#'   }
#' @export
#' @examples
#' # Example with simulated data
#' x <- seq(1, 100, by = 1)
#' y <- sin(x/10) + 0.1*x + rnorm(100, 0, 0.1)
#' result <- rolling_ball(y, wm = 10)
#' plot(x, y, type = "l", col = "blue", main = "Rolling Ball Correction")
#' lines(x, result$baseline, col = "red", lwd = 2)
#' lines(x, result$corrected, col = "green")
#' legend("topright", c("Original", "Baseline", "Corrected"),
#'        col = c("blue", "red", "green"), lty = 1)
rolling_ball <- function(x, wm, ws = 0) {

  # Parameter validation
  if (!is.numeric(x)) {
    stop("x must be a numeric vector")
  }

  if (length(x) < 3) {
    stop("x must have at least 3 data points")
  }

  if (missing(wm)) {
    stop("Parameter wm (window width) is required")
  }

  if (wm <= 0 || wm >= length(x)) {
    stop("wm must be positive and smaller than the length of x")
  }

  # Convert to ensure wm is integer
  wm <- as.integer(wm)
  ws <- as.integer(ws)

  # Rolling ball algorithm implementation
  n <- length(x)
  baseline <- numeric(n)

  # For each point, find the minimum in the rolling window
  for (i in 1:n) {
    # Define window boundaries
    start_idx <- max(1, i - wm)
    end_idx <- min(n, i + wm)

    # Find minimum value in the window
    window_vals <- x[start_idx:end_idx]
    baseline[i] <- min(window_vals, na.rm = TRUE)
  }

  # Optional baseline smoothing
  if (ws > 0 && ws < n/3) {
    baseline <- smooth_baseline(baseline, ws)
  }

  # Calculate corrected signal
  corrected <- x - baseline

  # Return results
  return(list(
    baseline = baseline,
    corrected = corrected,
    original = x
  ))
}

#' Baseline Smoothing
#'
#' Auxiliary function to smooth the baseline using moving average
#'
#' @param baseline Vector with the baseline
#' @param ws Smoothing window width
#' @return Smoothed numeric vector of the same length as input
smooth_baseline <- function(baseline, ws) {
  n <- length(baseline)
  smoothed <- numeric(n)

  for (i in 1:n) {
    start_idx <- max(1, i - ws)
    end_idx <- min(n, i + ws)
    smoothed[i] <- mean(baseline[start_idx:end_idx], na.rm = TRUE)
  }

  return(smoothed)
}

#' Enhanced Rolling Ball with Mathematical Morphology
#'
#' More sophisticated version of rolling ball using mathematical morphology concepts
#'
#' @param x Numeric vector containing the spectrum/signal values
#' @param radius Radius of the structuring ball
#' @param smooth Apply additional smoothing (logical)
#' @return A list with three components:
#'   \describe{
#'     \item{baseline}{Numeric vector containing the estimated baseline values using morphological operations}
#'     \item{corrected}{Numeric vector containing the baseline-corrected signal (original - baseline)}
#'     \item{original}{Numeric vector containing the original input signal}
#'   }
#' @export
rolling_ball_morphology <- function(x, radius, smooth = TRUE) {

  if (!is.numeric(x) || length(x) < 3) {
    stop("x must be a numeric vector with at least 3 data points")
  }

  if (radius <= 0 || radius >= length(x)/2) {
    stop("radius must be positive and smaller than half the length of x")
  }

  n <- length(x)
  radius <- as.integer(radius)

  # Mathematical morphology opening operation (erosion followed by dilation)
  # Erosion: find local minimum
  eroded <- numeric(n)
  for (i in 1:n) {
    start_idx <- max(1, i - radius)
    end_idx <- min(n, i + radius)
    eroded[i] <- min(x[start_idx:end_idx], na.rm = TRUE)
  }

  # Dilation: find local maximum of the eroded result
  baseline <- numeric(n)
  for (i in 1:n) {
    start_idx <- max(1, i - radius)
    end_idx <- min(n, i + radius)
    baseline[i] <- max(eroded[start_idx:end_idx], na.rm = TRUE)
  }

  # Optional smoothing
  if (smooth) {
    smooth_window <- max(3, radius %/% 2)
    baseline <- smooth_baseline(baseline, smooth_window)
  }

  corrected <- x - baseline

  return(list(
    baseline = baseline,
    corrected = corrected,
    original = x
  ))
}

#' Plot Rolling Ball Results
#'
#' Convenience function to visualize the results
#'
#' @param result Result from rolling_ball or rolling_ball_morphology function
#' @param title Plot title
#' @param x_values X-axis values (optional)
#' @return No return value, called for side effects (creates a plot)
#' @export
plot_rolling_ball <- function(result, title = "Rolling Ball Baseline Correction", x_values = NULL) {

  if (is.null(x_values)) {
    x_values <- seq_along(result$original)
  }

  # Save current par settings and restore on exit
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))

  # Plot configuration
  plot(x_values, result$original, type = "l", col = "blue", lwd = 1.5,
       main = title, xlab = "Index/Wavelength", ylab = "Intensity",
       ylim = range(c(result$original, result$corrected), na.rm = TRUE))

  graphics::lines(x_values, result$baseline, col = "red", lwd = 2)
  graphics::lines(x_values, result$corrected, col = "green", lwd = 1.5)

  graphics::legend("topright",
                   legend = c("Original", "Baseline", "Corrected"),
                   col = c("blue", "red", "green"),
                   lty = 1, lwd = c(1.5, 2, 1.5),
                   bg = "white")

  graphics::grid(col = "gray90", lty = 2)
}

#' Usage Example and Test
#'
#' Function to demonstrate the use of implemented functions
#'
#' @param verbose Logical indicating whether to print progress messages
#' @return A list containing two elements:
#'   \describe{
#'     \item{simple}{Results from the simple rolling ball method}
#'     \item{morphology}{Results from the mathematical morphology method}
#'   }
#' @export
#' @examples
#' \donttest{
#' # Run the demonstration
#' demo_results <- demo_rolling_ball()
#'
#' # Access results
#' simple_method <- demo_results$simple
#' morphology_method <- demo_results$morphology
#' }
demo_rolling_ball <- function(verbose = TRUE) {

  if (verbose) {
    message("Rolling Ball Baseline Correction Demonstration")
    message("============================================")
    message("")
  }

  # Create example data
  x <- seq(1, 200, by = 1)

  # Simulate a spectrum with curved baseline and peaks
  baseline_true <- 0.1 * x + 0.001 * x^2
  peaks <- 5 * exp(-((x - 50)/10)^2) +
    3 * exp(-((x - 100)/15)^2) +
    4 * exp(-((x - 150)/8)^2)
  noise <- stats::rnorm(length(x), 0, 0.2)
  y <- baseline_true + peaks + noise

  # Apply rolling ball correction
  if (verbose) {
    message("Applying Rolling Ball (simple method)...")
  }
  result1 <- rolling_ball(y, wm = 20)

  if (verbose) {
    message("Applying Rolling Ball (mathematical morphology)...")
  }
  result2 <- rolling_ball_morphology(y, radius = 25)

  # Save current par settings and restore on exit
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))

  # Plot results
  graphics::par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

  plot_rolling_ball(result1, "Rolling Ball - Simple Method", x)
  plot_rolling_ball(result2, "Rolling Ball - Mathematical Morphology", x)

  if (verbose) {
    message("")
    message("Demonstration completed!")
    message("Try different wm/radius values to optimize the results.")
  }

  return(list(simple = result1, morphology = result2))
}
