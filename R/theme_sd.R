#' Create a Custom ggplot2 Theme
#'
#' @description
#' Creates a customized ggplot2 theme based on theme_minimal with enhanced typography
#' and customized grid lines. This theme uses the Roboto family for base text and
#' Roboto Slab for titles by default.
#'
#' @param base_size Numeric. Base font size for the theme. Default is 11.
#' @param base_family Character. Font family for base text. Default is "Roboto".
#' @param title_family Character. Font family for plot titles. Default is "Roboto Slab".
#' @param axis_line Character. Where to draw axis lines. Options: "xy", "x", "y", "none". Default is "none".
#' @param axis_tick Character. Where to draw axis ticks. Options: "xy", "x", "y", "none". Default is "xy".
#' @param major_tick_length Numeric. Major tick length in mm. Minor ticks length is half. Default is 3 mm.
#' To display ticks inside the plot use a negative number.
#' @param grid Character. Where to draw grid lines. Options: "xy", "x", "y", "none". Default is "xy".
#' @param base_line_size Numeric. Line size for base elements. Default is `base_size / 22`.
#' @param base_rect_size Numeric. Rectangle size for base elements. Default is `base_size / 22`.
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
#'   theme_sd()
#'
#' # Modify grid lines to show only vertical grid
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_sd(grid = "y", axis_tick = "y", axis_line = "y")
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
#'   theme_sd(grid = "xy", major_tick_length = -3)
#' }
#'
#'
#' @importFrom systemfonts system_fonts register_variant
#' @importFrom stringr str_detect
#' @importFrom rlang inform
#'
#' @export
theme_sd <- function(base_size = 11,
                     base_family = "Helvetica",
                     title_family = "Helvetica",
                     axis_line = "none",
                     axis_tick = "xy",
                     major_tick_length = 3,
                     grid = "xy",
                     base_line_size = base_size / 22,
                     base_rect_size = base_size / 22) {
  # Argument validation
  axis_line <- rlang::arg_match(axis_line, c("xy", "x", "y", "none"))
  axis_tick <- rlang::arg_match(axis_tick, c("xy", "x", "y", "none"))
  grid <- rlang::arg_match(grid, c("xy", "x", "y", "none"))

  # Font handling
  available_fonts <- systemfonts::system_fonts()$family
  check_font <- function(font) if (font %in% available_fonts) font else ""

  final_base_family <- check_font(base_family)
  final_title_family <- check_font(title_family)

  # Inform user about missing fonts
  missing_fonts <- setdiff(c(base_family, title_family), available_fonts)
  if (length(missing_fonts) > 0) {
    msg <- paste0(
      "Using system default typefaces.\n",
      "For proper display, install these fonts:\n",
      paste(missing_fonts, collapse = ", "),
      "\nRestart R after installation."
    )
    rlang::inform(msg)
  }

  # Reusable styles
  grid_line <- ggplot2::element_line(color = shubham_cols[["dark_gray"]], linetype = "longdash")
  tick_length_unit <- ggplot2::unit(major_tick_length, "mm")

  # Base theme
  base_theme <- ggplot2::theme_bw(
    base_size = base_size,
    base_family = final_base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) + ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.grid.major = ggplot2::element_line(color = shubham_cols[["light_gray"]], linewidth = 0.2),
    panel.grid.minor = ggplot2::element_blank(),
    text = ggplot2::element_text(family = final_base_family, color = shubham_cols[["charcoal"]]),
    plot.title = ggplot2::element_text(
      family = final_title_family, size = base_size * 1.6, face = "bold",
      color = shubham_cols[["navy"]], margin = ggplot2::margin(b = base_size)
    ),
    plot.subtitle = ggplot2::element_text(size = base_size * 1.1, margin = ggplot2::margin(b = base_size * 0.9)),
    plot.caption = ggplot2::element_text(
      size = base_size * 0.9, color = shubham_cols[["dark_gray"]],
      margin = ggplot2::margin(t = base_size * 0.8)
    ),
    axis.title = ggplot2::element_text(size = base_size * 1.1, color = shubham_cols[["navy"]]),
    axis.text = ggplot2::element_text(size = base_size * 0.9, color = shubham_cols[["charcoal"]]),
    axis.minor.ticks.length = ggplot2::rel(0.5),
    legend.title = ggplot2::element_text(size = base_size * 1.1, color = shubham_cols[["navy"]]),
    legend.text = ggplot2::element_text(size = base_size * 0.9),
    legend.background = ggplot2::element_rect(fill = "white", color = NA),
    strip.text = ggplot2::element_text(size = base_size * 1.1, color = shubham_cols[["navy"]], face = "bold"),
    plot.margin = ggplot2::margin(t = base_size, r = base_size, b = base_size, l = base_size)
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
    "xy" = ggplot2::theme(panel.grid.major = grid_line),
    "x" = ggplot2::theme(
      panel.grid.major.x = grid_line,
      panel.grid.major.y = ggplot2::element_blank()
    ),
    "y" = ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = grid_line
    ),
    "none" = ggplot2::theme(panel.grid.major = ggplot2::element_blank())
  )

  return(base_theme +
           apply_theme(axis_line_settings, axis_line) +
           apply_theme(axis_tick_settings, axis_tick) +
           apply_theme(grid_settings, grid))
}
