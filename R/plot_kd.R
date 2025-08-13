#' Plot Kinetic Data with KD Lines
#'
#' @description
#' Creates a plot of kinetic data (k_on vs k_off) with optional KD (dissociation constant)
#' lines overlaid. The plot is generated on a log-log scale with customizable axis limits
#' and number of KD lines.
#'
#' @param data A data frame containing columns:
#'   \itemize{
#'     \item k_off: Off rate constants (s^-1)
#'     \item k_on: On rate constants (M^-1 s^-1)
#'     \item sample_id: Sample identifiers for coloring points
#'   }
#' @param x_limits Numeric vector of length 2 specifying the limits for k_off axis (default: c(1e-6, 1e-0))
#' @param y_limits Numeric vector of length 2 specifying the limits for k_on axis (default: c(1e2, 1e7))
#' @param show_KD Logical indicating whether to show KD lines (default: TRUE)
#' @param n_lines Integer specifying the number of KD lines to display (default: 7)
#'
#' @return A ggplot2 object containing the kinetics plot
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   k_off = c(1e-3, 1e-4, 1e-2),
#'   k_on = c(1e5, 1e6, 1e4),
#'   sample_id = c("A", "B", "C")
#' )
#' plot_kd(data)
#' }
#'
#' @importFrom ggplot2 ggplot geom_point scale_x_log10 scale_y_log10 labs geom_line annotate aes
#' @importFrom purrr map_df
#'
plot_kd <- function(
    data,
    x_limits = c(1e-6, 1e-0),
    y_limits = c(1e2, 1e7),
    show_KD = TRUE,
    n_lines = 7) {

  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  required_cols <- c("k_off", "k_on", "sample_id")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in data: ", paste(missing_cols, collapse = ", "))
  }

  if (!is.numeric(data$k_off) || !is.numeric(data$k_on)) {
    stop("'k_off' and 'k_on' must be numeric columns")
  }

  if (any(data$k_off <= 0) || any(data$k_on <= 0)) {
    stop("'k_off' and 'k_on' values must be positive")
  }

  if (!is.numeric(x_limits) || length(x_limits) != 2 || x_limits[1] >= x_limits[2]) {
    stop("'x_limits' must be a numeric vector of length 2 with x_limits[1] < x_limits[2]")
  }

  if (!is.numeric(y_limits) || length(y_limits) != 2 || y_limits[1] >= y_limits[2]) {
    stop("'y_limits' must be a numeric vector of length 2 with y_limits[1] < y_limits[2]")
  }

  if (!is.logical(show_KD)) {
    stop("'show_KD' must be logical (TRUE/FALSE)")
  }

  if (!is.numeric(n_lines) || n_lines < 1 || n_lines != round(n_lines)) {
    stop("'n_lines' must be a positive integer")
  }

  # Helper function to format KD values into nice labels
  format_KD_label <- function(kd) {
    if (kd < 1e-9) {
      return(paste0(round(kd * 1e12, 1), "pM"))
    } else if (kd < 1e-6) {
      return(paste0(round(kd * 1e9, 1), "nM"))
    } else if (kd < 1e-3) {
      return(paste0(round(kd * 1e6, 1), "\u00B5M"))
    } else if (kd < 1e-0) {
      return(paste0(round(kd * 1e3, 1), "mM"))
    } else if (kd < 1e3) {
      return(paste0(round(kd * 1e0, 1), "M"))
    }
  }

  # Helper function to generate KD lines
  generate_KD_lines <- function(kd) {
    k_off <- 10^seq(log10(x_limits[1]), log10(x_limits[2]), length.out = 100)
    k_on <- k_off/kd
    data.frame(k_off = k_off, k_on = k_on, kd = kd)
  }

  # Calculate KD values and generate lines
  KD_values <- calculate_KD_range(data, n_lines)
  KD_lines <- purrr::map_df(KD_values, generate_KD_lines, .id = "line_id")

  # Calculate annotation positions
  x_pos_left <- rep(x_limits[1], length(KD_values))
  y_pos_left <- 1.2/KD_values * x_pos_left

  x_pos_right <- rep(x_limits[2], length(KD_values))
  y_pos_right <- 0.8/KD_values * x_pos_right

  # Create base plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = data,
      ggplot2::aes(x = k_off, y = k_on, color = sample_id),
      size = 3
    ) +
    ggplot2::scale_x_log10(
      limits = x_limits,
      breaks = 10^seq(log10(x_limits[1]), log10(x_limits[2]), 1)
    ) +
    ggplot2::scale_y_log10(
      limits = y_limits,
      breaks = 10^seq(log10(y_limits[1]), log10(y_limits[2]), 1)
    ) +
    ggplot2::labs(
      x = expression(k[off]~(s^-1)),
      y = expression(k[on]~(M^-1~s^-1))
    )

  # Add KD lines and annotations if requested
  if (show_KD) {
    p <- p +
      ggplot2::geom_line(
        data = KD_lines,
        ggplot2::aes(x = k_off, y = k_on, group = line_id),
        linetype = "dashed", color = "gray70"
      ) +
      ggplot2::annotate(
        geom = "text",
        x = x_pos_left,
        y = y_pos_left,
        label = sapply(KD_values, format_KD_label),
        hjust = -0.2,
        size = 3,
        angle = 45
      ) +
      ggplot2::annotate(
        geom = "text",
        x = x_pos_right,
        y = y_pos_right,
        label = sapply(KD_values, format_KD_label),
        hjust = 1.2,
        size = 3,
        angle = 45
      )
  }

  return(p)
}

#' Calculate KD Range for Plotting
#'
#' @description
#' Helper function to calculate an appropriate range of KD values for plotting,
#' based on the input kinetic data.
#'
#' @param data A data frame containing k_off and k_on columns
#' @param n_lines Integer specifying the number of KD lines to generate
#'
#' @return A numeric vector of KD values
#'
#' @keywords internal
calculate_KD_range <- function(data, n_lines) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (!all(c("k_off", "k_on") %in% names(data))) {
    stop("'data' must contain 'k_off' and 'k_on' columns")
  }

  if (!is.numeric(n_lines) || n_lines < 1 || n_lines != round(n_lines)) {
    stop("'n_lines' must be a positive integer")
  }

  # Calculate KD for each data point
  kd_points <- data$k_off / data$k_on

  # Get range with padding
  kd_min <- min(kd_points) / 10  # Extend one order of magnitude lower
  kd_max <- max(kd_points) * 1000  # Extend one order of magnitude higher

  # Generate evenly spaced values in log space
  KD_values <- 10^seq(log10(kd_min), log10(kd_max), length.out = n_lines)

  return(KD_values)
}
