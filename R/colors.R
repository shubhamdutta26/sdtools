#' Shubham Colors
#'
#' A collection of shubhamd colors for use in plots and visualizations
#'
#' @format A list of color hex codes
#' @export
shubham_cols <- c(
  navy          = "#2D3047",
  green         = "#2F9D72",
  blue          = "#2572B2",
  yellow        = "#CBBA44",
  purple        = "#58508D",
  light_gray    = "#E2E4E6",
  dark_gray     = "#9DA3A7",
  charcoal      = "#434A4F",
  black         = "#000000"
)

#' Display Color Palettes from the Shubham Theme
#'
#' This function creates a visual display of color palettes from the Shubham theme.
#' It shows color swatches along with their names and hexadecimal codes.
#'
#' @param palette Character string specifying which palette to display.
#'   Must be either "main" or "grays".
#'
#' @return Invisibly returns NULL. The function is called for its side effect
#'   of creating a plot.
#'
#' @details
#' The function creates a plot showing color swatches for either the main palette
#' or the grays palette. Each swatch displays the color name and its hexadecimal
#' code. Text color (black or white) is automatically selected based on the
#' background color's luminance for optimal readability.
#'
#' The main palette includes: navy, green, blue, yellow, and purple
#' The grays palette includes: black, navy, charcoal, dark_gray, and light_gray
#'
#' @examples
#' # Display main colors
#' display_colors_d("main")
#'
#' # Display gray colors
#' display_colors_d("grays")
#'
#' @importFrom graphics plot.new par plot.window text rect
#' @importFrom grDevices col2rgb
#' @importFrom rlang abort
#'
#' @export
display_colors_d <- function(palette = c("main", "grays")) {

  # Split into main and grays palettes
  color_palettes <- list(
    main = shubham_cols[c("navy", "green", "blue", "yellow", "purple")],
    grays = shubham_cols[c("black", "charcoal", "dark_gray", "light_gray")]
  )

  # Check if palette argument is valid
  if (!palette %in% names(color_palettes)) {
    rlang::abort("palette must be either 'main' or 'grays'", call = NULL)
  }

  # Get colors and names for the selected palette
  colors <- unlist(color_palettes[[palette]])
  names <- names(colors)
  n_colors <- length(colors)

  # Function to determine text color based on background
  get_text_color <- function(hex) {
    rgb <- grDevices::col2rgb(hex)
    luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
    ifelse(luminance > 0.5, "black", "white")
  }

  text_colors <- sapply(colors, get_text_color)

  graphics::plot.new()
  # Set margins
  graphics::par(mar = c(1, 1, 1, 1))
  # Set the plot region
  graphics::plot.window(xlim = c(0, 1),
                        ylim = c(0, n_colors + 1),
                        xaxs = "i",
                        yaxs = "i")

  # Add title
  graphics::text(0.5, n_colors + 0.5,
                 paste(toupper(substring(palette, 1, 1)),
                       substring(palette, 2),
                       " colors",
                       sep=""),
                 cex = 1.2,
                 font = 2)

  # Draw color rectangles and text
  for (i in 1:n_colors) {
    # Draw rectangle
    graphics::rect(0.1, n_colors - i + 0.2,
                   0.9, n_colors - i + 0.8,
                   col = colors[i],
                   border = "grey50")
    # Add color name
    graphics::text(0.2, n_colors - i + 0.5,
                   names[i],
                   adj = 0,
                   col = text_colors[i],
                   cex = 1)
    # Add hex code
    graphics::text(0.8, n_colors - i + 0.5,
                   colors[i],
                   adj = 1,
                   col = text_colors[i],
                   cex = 1)
  }

  invisible(NULL)
}

#' Get Shubham's Color Palette
#'
#' @description Returns a vector of hex color codes from Shubham's custom color palette.
#' If no specific colors are requested, returns the complete palette.
#'
#' @param ... Character vector of color names to retrieve. Valid options are:
#'   "navy", "green", "blue", "yellow", "purple", "light_gray", "dark_gray",
#'   "charcoal", "black"
#'
#' @return A named character vector of hex color codes
#' @export
#' @examples
#' # Get all colors
#' shubham_colors()
#'
#' # Get specific colors
#' shubham_colors("navy", "green")
shubham_colors <- function(...) {

  # if no colors are specified, return all
  cols <- c(...)
  if (is.null(cols))  return (shubham_cols)

  # if colors are specified, return those
  shubham_cols[cols]
}

#' Create a Discrete Color Palette Function
#'
#' @description Creates a function that returns a vector of colors from Shubham's
#' discrete color palettes.
#'
#' @param palette Character string indicating the palette type.
#'   Options are "main" or "grays"
#' @param reverse Logical. If TRUE, reverses the order of colors
#'
#' @return A function that takes an integer argument (n) and returns n colors
#' @export
#' @examples
#' # Get 3 colors from main palette
#' pal <- shubham_pal_d()
#' pal(3)
#'
#' # Get reversed gray palette
#' pal <- shubham_pal_d("grays", reverse = TRUE)
#' pal(4)
shubham_pal_d <- function(palette = "main", reverse = FALSE) {

  # nested function to return colors via `shubham_pal_d()(n)`
  function(n) {

    # check if number of colors is sufficient
    if(n > 5) rlang::abort('Palettes only contain 4 colors.', call = NULL)

    # check arguments
    if (!palette %in% c("main", "grays")) rlang::abort('`palette` should be "main" or "grays".', call = NULL)
    if (!is.logical(reverse) & !is.numeric(reverse)) rlang::abort('`reverse` should be logical or numeric.', call = NULL)

    # define palette styles
    if (palette == "main") { pal <- shubham_colors("navy", "green", "blue", "yellow", "purple")[1:n] }
    if (palette == "grays") { pal <- shubham_colors("black", "charcoal", "dark_gray", "light_gray")[1:n] }

    # return unnamed vector of color codes
    pal <- unname(pal)

    # check reverse argument
    if (reverse) rev(pal) else pal
  }
}

#' Discrete Color Scale for ggplot2
#'
#' @description Creates a discrete color scale for ggplot2 using Shubham's color palettes
#'
#' @param palette Character string indicating the palette type.
#'   Options are "main" or "grays"
#' @param reverse Logical. If TRUE, reverses the order of colors
#' @param ... Additional arguments passed to ggplot2::discrete_scale()
#'
#' @return A ggplot2 color scale
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_shubham_d()
scale_color_shubham_d <- function(palette = "main", reverse = FALSE, ...) {

  # check arguments
  if (!palette %in% c("main", "grays")) rlang::abort('`palette` should be "main" or "grays".', call = NULL)
  if (!is.logical(reverse) & !is.numeric(reverse)) rlang::abort('reverse should be logical or numeric.', call = NULL)

  # retrieve color set
  pal <- shubham_pal_d(palette = palette, reverse = reverse)

  # apply to discrete scale
  ggplot2::discrete_scale("colour", paste0("shubham_", palette), palette = pal, ...)
}

#' Discrete Fill Scale for ggplot2
#'
#' @description Creates a discrete fill scale for ggplot2 using Shubham's color palettes
#'
#' @param palette Character string indicating the palette type.
#'   Options are "main" or "grays"
#' @param reverse Logical. If TRUE, reverses the order of colors
#' @param ... Additional arguments passed to ggplot2::discrete_scale()
#'
#' @return A ggplot2 fill scale
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl), fill = factor(am))) +
#'   geom_bar() +
#'   scale_fill_shubham_d()
scale_fill_shubham_d <- function(palette = "main", reverse = FALSE, ...) {

  # check arguments
  if (!palette %in% c("main", "grays")) rlang::abort('`palette` should be "main" or "grays".', call = NULL)
  if (!is.logical(reverse) & !is.numeric(reverse)) rlang::abort('reverse should be logical or numeric.', call = NULL)

  # retrieve color set
  pal <- shubham_pal_d(palette = palette, reverse = reverse)

  # apply to discrete scale
  ggplot2::discrete_scale("fill", paste0("shubham_", palette), palette = pal, ...)
}

#' Create a Continuous Color Palette Function
#'
#' @description Creates a function that returns a continuous color palette from
#' Shubham's color schemes
#'
#' @param palette Character string indicating the palette type.
#'   Options are "main" or "grays"
#' @param reverse Logical. If TRUE, reverses the order of colors
#' @param ... Additional arguments passed to grDevices::colorRampPalette()
#'
#' @return A function that takes an integer argument (n) and returns n colors
#' @export
#' @examples
#' # Get 100 colors interpolated from main palette
#' pal <- shubham_pal_c()
#' pal(100)
shubham_pal_c <- function(palette = "main", reverse = FALSE, ...) {

  # check arguments
  if (!palette %in% c("main", "grays")) rlang::abort('`palette` should be "main" or "grays".', call = NULL)
  if (!is.logical(reverse) & !is.numeric(reverse)) rlang::abort('`reverse` should be logical or numeric.')

  # define palette styles
  shubham_palettes <- list(
    main    = shubham_colors("navy", "green", "yellow"),
    grays   = shubham_colors("black", "charcoal", "dark_gray", "light_gray")
  )

  # retrieve color set as unnamed vector
  pal <- shubham_palettes[[palette]]
  pal <- unname(pal)

  # check reverse argument
  if (reverse) pal <- rev(pal)

  # create a color gradient with n colors
  grDevices::colorRampPalette(pal, ...)
}

#' Continuous Color Scale for ggplot2
#'
#' @description Creates a continuous color scale for ggplot2 using Shubham's color palettes
#'
#' @param palette Character string indicating the palette type.
#'   Options are "main" or "grays"
#' @param reverse Logical. If TRUE, reverses the order of colors
#' @param ... Additional arguments passed to ggplot2::scale_color_gradientn()
#'
#' @return A ggplot2 color scale
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(faithfuld, aes(waiting, eruptions, color = density)) +
#'   geom_point() +
#'   scale_color_shubham_c()
scale_color_shubham_c <- function(palette = "main", reverse = FALSE, ...) {

  # check function arguments
  if (!palette %in% c("main", "grays")) rlang::abort('Palette should be "main" or "grays".', call = NULL)
  if (!is.logical(reverse) & !is.numeric(reverse)) rlang::abort('reverse should be logical or numeric.', call = NULL)

  # apply color set to ggplot's gradientn scale
  pal <- shubham_pal_c(palette = palette, reverse = reverse)
  ggplot2::scale_color_gradientn(colours = pal(256), ...)
}

#' Continuous Fill Scale for ggplot2
#'
#' @description Creates a continuous fill scale for ggplot2 using Shubham's color palettes
#'
#' @param palette Character string indicating the palette type.
#'   Options are "main" or "grays"
#' @param reverse Logical. If TRUE, reverses the order of colors
#' @param ... Additional arguments passed to ggplot2::scale_fill_gradientn()
#'
#' @return A ggplot2 fill scale
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_tile() +
#'   scale_fill_shubham_c()
scale_fill_shubham_c <- function(palette = "main", reverse = FALSE, ...) {

  if (!palette %in% c("main", "grays")) stop('Palette should be "main" or "grays".')
  if (!is.logical(reverse) & !is.numeric(reverse)) stop('reverse should be logical or numeric.')

  pal <- shubham_pal_c(palette = palette, reverse = reverse)
  ggplot2::scale_fill_gradientn(colours = pal(256), ...)
}

#' Display Continuous Color Gradient
#'
#' Creates a visual display of a continuous color gradient between specified colors
#' from the Shubham theme.
#'
#' @param ... Character strings specifying color names from the Shubham theme.
#'   Must be valid color names from shubham_cols.
#'
#' @return Invisibly returns NULL. The function is called for its side effect
#'   of creating a plot.
#'
#' @details
#' The function creates a horizontal color gradient between the specified colors.
#' It interpolates between the colors to create a smooth transition and displays
#' the color names at their respective positions along the gradient.
#'
#' The gradient is created using colorRampPalette for smooth color transitions.
#' Color names are displayed above their starting positions in the gradient.
#'
#' @examples
#' # Create gradient between three colors
#' display_continuous_colors("navy", "green", "yellow")
#'
#' # Create gradient between two colors
#' display_continuous_colors("navy", "yellow")
#'
#' @importFrom graphics plot.new par plot.window rect text
#' @importFrom grDevices colorRampPalette
#' @importFrom rlang abort
#'
#' @export
display_continuous_colors <- function(...) {
  # Get color names from arguments
  color_names <- unlist(list(...))

  # Validate that all colors exist in shubham_cols
  if (!all(color_names %in% names(shubham_cols))) {
    invalid_colors <- color_names[!color_names %in% names(shubham_cols)]
    rlang::abort(
      sprintf("Invalid color names: %s",
              paste(invalid_colors, collapse = ", ")),
      call = NULL
    )
  }

  # Get the actual colors from shubham_cols
  colors <- unlist(shubham_cols[color_names])
  n_colors <- length(colors)

  if (n_colors < 2) {
    rlang::abort("At least two colors must be specified", call = NULL)
  }

  # Create color ramp function
  n_segments <- 100
  color_ramp <- grDevices::colorRampPalette(colors)
  gradient_colors <- color_ramp(n_segments)

  # Set up the plot
  graphics::plot.new()
  graphics::par(mar = c(2, 1, 3, 1))
  graphics::plot.window(xlim = c(0, 1),
                        ylim = c(0, 1),
                        xaxs = "i",
                        yaxs = "i")

  # Draw gradient rectangles
  rect_width <- 1 / n_segments
  for (i in 1:n_segments) {
    graphics::rect(
      (i - 1) * rect_width, 0.1,
      i * rect_width, 0.5,
      col = gradient_colors[i],
      border = NA
    )
  }

  invisible(NULL)
}

#' Create color palette with specified number of colors
#'
#' Creates a color palette with a specified number of evenly spaced colors
#' from a continuous gradient between given colors.
#'
#' @param ... Character strings specifying color names from the Shubham theme.
#' @param n Integer specifying the number of colors to generate.
#'
#' @return A character vector of hex color codes.
#' @examples
#' # Get 5 colors between navy and yellow
#' get_n_colors("navy", "yellow", n = 5)
#'
#' # Get 10 colors between navy, green, and yellow
#' get_n_colors("navy", "green", "yellow", n = 10)
#'
#' @importFrom grDevices colorRampPalette
#' @export
get_n_colors <- function(..., n) {
  # Get color names from arguments
  color_names <- unlist(list(...))

  # Validate that all colors exist in shubham_cols
  if (!all(color_names %in% names(shubham_cols))) {
    invalid_colors <- color_names[!color_names %in% names(shubham_cols)]
    rlang::abort(
      sprintf("Invalid color names: %s",
              paste(invalid_colors, collapse = ", ")),
      call = NULL
    )
  }

  # Get the actual colors from shubham_cols
  colors <- unlist(shubham_cols[color_names])

  if (length(colors) < 2) {
    rlang::abort("At least two colors must be specified", call = NULL)
  }

  # Create and return color palette
  grDevices::colorRampPalette(colors)(n)
}
