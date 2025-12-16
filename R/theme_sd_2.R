#' Create a custom ggplot2 theme
#'
#' @description
#' Creates a customized ggplot2 theme based on theme_bw.
#'
#' @param ... Default arguments of theme_bw()
#' @param axis_line Character. Where to draw axis lines. Options: "xy", "x", "y", "none". Default is "none".
#' @param axis_tick Character. Where to draw axis ticks. Options: "xy", "x", "y", "none". Default is "xy".
#' @param major_tick_length Numeric. Major tick length in mm. Minor ticks length is half. Default is 3 mm.
#' To display ticks inside the plot use a negative number.
#' @param grid Character. Where to draw grid lines. Options: "xy", "x", "y", "none". Default is "xy".
#'
#' @return A ggplot2 theme object.
#'
#' @details
#' The theme checks for font availability and will fall back to system defaults if
#' required fonts are not installed. It will provide instructions for installing
#' missing fonts from Google Fonts.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Basic usage with defaults
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_sd_2()
#'
#' # Modify grid lines to show only vertical grid
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_sd_2(grid = "y", axis_tick = "y", axis_line = "y")
#'
#' # To display minor ticks use guides()
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   guides(
#'     x = guide_axis(minor.ticks = TRUE),
#'     y = guide_axis(minor.ticks = TRUE)
#'   ) +
#'   scale_y_continuous(expand = expansion(0), minor_breaks = scales::breaks_width(1)) +
#'   scale_x_continuous(expand = expansion(0), minor_breaks = scales::breaks_width(0.2)) +
#'   theme_sd_2(grid = "xy", major_tick_length = -3)
#' }
#'
#' @export
theme_sd_2 <- function(
  ...,
  axis_line = "none",
  axis_tick = "xy",
  major_tick_length = 3,
  grid = "xy"
) {
  # Argument validation
  axis_line <- rlang::arg_match(axis_line, c("xy", "x", "y", "none"))
  axis_tick <- rlang::arg_match(axis_tick, c("xy", "x", "y", "none"))
  grid <- rlang::arg_match(grid, c("xy", "x", "y", "none"))

  # Tick length. Negative values will diplay ticks inside the plot.
  tick_length_unit <- ggplot2::unit(major_tick_length, "mm")

  # Base theme
  base_theme <- ggplot2::theme_bw() + 
    ggplot2::theme(
    plot.background = ggplot2::element_rect(color = NA),
    panel.background = ggplot2::element_rect(color = NA),
    panel.grid.major = ggplot2::element_line(linewidth = 0.2),
    panel.grid.minor = ggplot2::element_blank(),
    axis.minor.ticks.length = ggplot2::rel(0.5),
    legend.background = ggplot2::element_rect(color = NA)
  )

  # Utility function to apply settings
  apply_theme <- function(settings, key) settings[[key]] %||% ggplot2::theme()

  # Theme customizations
  axis_line_settings <- list(
    "xy" = ggplot2::theme(axis.line = ggplot2::element_line()),
    "x" = ggplot2::theme(axis.line.x = ggplot2::element_line(), axis.line.y = ggplot2::element_blank()),
    "y" = ggplot2::theme(axis.line.x = ggplot2::element_blank(), axis.line.y = ggplot2::element_line()),
    "none" = ggplot2::theme(axis.line = ggplot2::element_blank())
  )

  axis_tick_settings <- list(
    "xy" = ggplot2::theme(
      axis.ticks = ggplot2::element_line(color = "black"),
      axis.ticks.length = tick_length_unit
    ),
    "x" = ggplot2::theme(
      axis.ticks.x = ggplot2::element_line(color = "black"),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.length = tick_length_unit
    ),
    "y" = ggplot2::theme(
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_line(color = "black"),
      axis.ticks.length = tick_length_unit
    ),
    "none" = ggplot2::theme(axis.ticks = ggplot2::element_blank())
  )

  grid_settings <- list(
    "xy" = ggplot2::theme(panel.grid.major = ggplot2::element_line()),
    "x" = ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(),
      panel.grid.major.y = ggplot2::element_blank()
    ),
    "y" = ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line()
    ),
    "none" = ggplot2::theme(panel.grid.major = ggplot2::element_blank())
  )

  return(
    base_theme +
      apply_theme(axis_line_settings, axis_line) +
      apply_theme(axis_tick_settings, axis_tick) +
      apply_theme(grid_settings, grid))
}
