#' Create a Custom ggplot2 Theme
#'
#' @description
#' Creates a customized ggplot2 theme based on theme_minimal with enhanced typography
#' and customized grid lines. This theme uses the Roboto family for base text and
#' Roboto Slab for titles by default.
#'
#' @param base_size Base font size in points (default: 9)
#' @param base_family Base font family for regular text (default: "Roboto")
#' @param title_family Font family for titles (default: "Roboto Slab")
#' @param grid Controls the grid lines. One of "xy" (both grids), "x" (only x grid),
#'   "y" (only y grid), or "none" (no grid) (default: "xy")
#' @param base_line_size Base size for line elements (default: base_size/22)
#' @param base_rect_size Base size for rectangular elements (default: base_size/22)
#'
#' @return A ggplot2 theme object
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
#'   theme_sd(grid = "y")
#' }
#'
#' @importFrom systemfonts system_fonts register_variant
#' @importFrom stringr str_detect
#' @importFrom rlang inform
#'
#' @export
theme_sd <- function(base_size =11,
                          base_family = "Roboto",
                          title_family = "Roboto Slab",
                          grid = "xy",
                          base_line_size = base_size/22,
                          base_rect_size = base_size/22) {

  # Validate grid argument
  grid <- rlang::arg_match(grid, values = c("xy", "x", "y", "none"))

  # Initialize variables
  unavailable <- vector("character")
  final_base_family <- ""
  final_title_family <- ""

  # Check base family
  if (sum(grepl(base_family, systemfonts::system_fonts()$family)) > 0) {
    final_base_family <- base_family
  } else {
    unavailable <- c(unavailable, base_family)
  }

  # Check title family
  if (sum(grepl(title_family, systemfonts::system_fonts()$family)) > 0) {
    systemfonts::register_variant(
      name = paste0(title_family, " Bold"),
      family = title_family,
      weight = "bold"
    )
    final_title_family <- paste0(title_family, " Bold")
  } else {
    unavailable <- c(unavailable, title_family)
  }

  # Inform about unavailable fonts
  if (length(unavailable) > 0) {
    unavailable <- unique(unavailable)  # Remove duplicates
    unavailable <- data.frame(
      name = unavailable,
      url = paste0("https://fonts.google.com/specimen/", sub(" ", "+", unavailable))
    )
    rlang::inform(c(
      "Using system default typefaces.",
      "i" = "For proper use, please install the following typeface(s):",
      paste0("  * ", unavailable$name, ": ", unavailable$url, collapse = "\n"),
      "i" = "Then restart your R session."
    ))
  }

  # Create base theme
  out <- ggplot2::theme_minimal(
    base_size = base_size,
    base_family = final_base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) +
    ggplot2::theme(
      # Plot Background
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      # Grid lines
      panel.grid.major = ggplot2::element_line(color = shubham_cols[["light_gray"]], linewidth = 0.2),
      panel.grid.minor = ggplot2::element_line(color = shubham_cols[["light_gray"]], linewidth = 0.1),
      # Text elements
      text = ggplot2::element_text(family = final_base_family, color = shubham_cols[["charcoal"]]),
      plot.title = ggplot2::element_text(
        family = final_title_family,
        size = base_size * 1.6,
        face = "bold",
        color = shubham_cols[["navy"]],
        margin = ggplot2::margin(b = base_size)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size * 1.1,
        margin = ggplot2::margin(b = base_size * 0.9)
      ),
      plot.caption = ggplot2::element_text(
        size = base_size * 0.9,
        color = shubham_cols[["dark_gray"]],
        margin = ggplot2::margin(t = base_size * 0.8)
      ),
      # Axis formatting
      axis.title = ggplot2::element_text(
        size = base_size * 1.1,
        color = shubham_cols[["navy"]]
      ),
      axis.text = ggplot2::element_text(
        size = base_size * 0.9,
        color = shubham_cols[["charcoal"]]
      ),
      axis.ticks = ggplot2::element_line(color = shubham_cols[["dark_gray"]], linewidth = 0.2),
      # Legend formatting
      legend.title = ggplot2::element_text(
        size = base_size * 1.1,
        color = shubham_cols[["navy"]]
      ),
      legend.text = ggplot2::element_text(size = base_size * 0.9),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      # Facet formatting
      strip.text = ggplot2::element_text(
        size = base_size * 1.1,
        color = shubham_cols[["navy"]],
        face = "bold"
      ),
      # Add padding around the plot
      plot.margin = ggplot2::margin(t = base_size,
                                    r = base_size,
                                    b = base_size,
                                    l = base_size)
    )

  # Apply grid settings based on argument
  out <- switch(grid,
                "xy" = out + ggplot2::theme(
                  panel.grid.major.x = ggplot2::element_line(color = shubham_cols[["light_gray"]]),
                  panel.grid.major.y = ggplot2::element_line(color = shubham_cols[["light_gray"]]),
                  axis.ticks.x = ggplot2::element_blank(),
                  axis.ticks.y = ggplot2::element_blank(),
                  axis.ticks.length.x = ggplot2::unit(base_size/6, "pt"),
                  axis.ticks.length.y = ggplot2::unit(base_size/4, "pt")
                ),
                "x" = out + ggplot2::theme(
                  panel.grid.major.x = ggplot2::element_line(color = shubham_cols[["light_gray"]]),
                  panel.grid.major.y = ggplot2::element_blank(),
                  panel.grid.minor.y = ggplot2::element_blank(),
                  axis.ticks.x = ggplot2::element_blank(),
                  axis.ticks.length.x = ggplot2::unit(base_size/6, "pt")
                ),
                "y" = out + ggplot2::theme(
                  panel.grid.major.x = ggplot2::element_blank(),
                  panel.grid.minor.x = ggplot2::element_blank(),
                  panel.grid.major.y = ggplot2::element_line(color = shubham_cols[["light_gray"]]),
                  axis.ticks.y = ggplot2::element_blank(),
                  axis.ticks.length.y = ggplot2::unit(base_size/4, "pt")
                ),
                "none" = out + ggplot2::theme(
                  panel.grid.major = ggplot2::element_blank(),
                  panel.grid.minor = ggplot2::element_blank()
                )
  )

  return(out)
}
