#' Access sd color values
#'
#' Returns one or more hex color codes from the sd palette by name or index. 
#' If no arguments are supplied, the first six (discrete) colors are returned.
#'
#' @param ... Color names (e.g. `"green"`, `"pink"`) or integer indices 
#'   referring to positions in the internal palette vector.
#'
#' @return A character vector of hex color codes.
#' @export
#' @examples
#' sd_colors()
#' sd_colors("green", "pink")
#' sd_colors(1, 3, 5)
sd_colors <- function(...) {
  
  # define colors
  sd_cols <- c(
    `green`   = "#00a5a6",
    `pink`    = "#e03d90",
    `orange`  = "#fc6538",
    `purple`  = "#6a3ecb",
    `blue`    = "#0058dc",
    `brown`   = "#a75237",
    
    `blue1`   = "#111c32",
    `blue2`   = "#345697",
    `blue3`   = "#5790fc",
    `blue4`   = "#83adfc",
    `blue5`   = "#b0cbfd",
    `blue6`   = "#dde8fe",
    
    `gray1`   = "#2d2d2e",
    `gray2`   = "#515252",
    `gray3`   = "#757677",
    `gray4`   = "#999b9c",
    `gray5`   = "#bdbfc1",
    `gray6`   = "#E2E4E6"
  )
  
  # if no colors are specified, return all
  cols <- c(...)
  if (is.null(cols))  return (sd_cols[1:6])
  
  # if colors are specified, return those
  sd_cols[cols]
}

#' Discrete sd palette function
#'
#' Generates a function that returns a discrete vector of sd colors for use 
#' with ggplot2 discrete scales.
#'
#' @param palette Character string, one of `"default"`, `"blues"`, or `"grays"`.
#' @param reverse Logical; if `TRUE`, the palette is reversed.
#'
#' @return A function that takes an integer \code{n} and returns \code{n}
#'   hex color codes.
#' @export
#' @examples
#' pal <- sd_pal_d("default")
#' pal(3)
sd_pal_d <- function(palette = "default", reverse = FALSE) {
  
  # nested function to return colors via `dubois_pal_d()(n)`
  function(n) {
    
    # check if number of colors is sufficient
    if(n > 6) stop('Palettes only contain 6 colors')
    
    # check arguments
    if (!palette %in% c("default", "blues", "grays")) stop('palette should be "default", "grays" or "blues".')
    if (!is.logical(reverse) & !is.numeric(reverse)) stop('reverse should be logical or numeric')
    
    # define palette styles
    if (palette == "default") { pal <- sd_colors(1:6)[1:n] }
    if (palette == "blues") { pal <- sd_colors(7:12)[1:n] }
    if (palette == "grays") { pal <- sd_colors(13:18)[1:n] }
    
    # return unnamed vector of color codes
    pal <- unname(pal)
    
    # check reverse argument
    if (reverse) rev(pal) else pal
  }
}

#' Discrete sd color scale
#'
#' Discrete color scale for ggplot2 using sd palettes.
#'
#' @param palette Character string, one of `"default"`, `"blues"`, or `"grays"`.
#' @param reverse Logical; if `TRUE`, the palette is reversed.
#' @param ... Additional arguments passed to \code{ggplot2::discrete_scale()}.
#'
#' @return A ggplot2 scale object.
#' @export
#' @examples
#' \dontrun{
#' ggplot(mtcars, aes(wt, qsec, color = factor(gear))) +
#'   geom_point(size = 5) +
#'   scale_color_sd_d()
#' }
scale_color_sd_d <- function(palette = "default", reverse = FALSE, ...) {
  
  # check arguments
  if (!palette %in% c("default", "blues", "grays")) stop('palette should be "default", "grays" or "blues".')
  if (!is.logical(reverse) & !is.numeric(reverse)) stop('reverse should be logical or numeric')
  
  # retrieve color set
  pal <- sd_pal_d(palette = palette, reverse = reverse)
  
  # apply to discrete scale
  ggplot2::discrete_scale(aesthetics = "color", palette = pal, ...)
}

#' Discrete sd fill scale
#'
#' Discrete fill scale for ggplot2 using sd palettes.
#'
#' @inheritParams scale_color_sd_d
#'
#' @return A ggplot2 scale object.
#' @export
#' @examples
#' \dontrun{
#' ggplot(mtcars, aes(factor(cyl), fill = factor(gear))) +
#'   geom_bar() +
#'   scale_fill_sd_d()
#' }
scale_fill_sd_d <- function(palette = "default", reverse = FALSE, ...) {
  
  # check arguments
  if (!palette %in% c("default", "blues", "grays")) stop('palette should be "default", "grays" or "blues".')
  if (!is.logical(reverse) & !is.numeric(reverse)) stop('reverse should be logical or numeric')
  
  # retrieve color set
  pal <- sd_pal_d(palette = palette, reverse = reverse)
  
  # apply to discrete scale
  ggplot2::discrete_scale(aesthetics = "fill", palette = pal, ...)
}

#' Continuous sd palette function
#'
#' Returns a colorRampPalette based on an sd palette for continuous scales.
#'
#' @param palette Character string, one of `"default"`, `"blues"`, or `"grays"`.
#' @param reverse Logical; if `TRUE`, the palette is reversed.
#' @param ... Additional arguments passed to \code{grDevices::colorRampPalette()}.
#'
#' @return A function from \code{grDevices::colorRampPalette()} that generates
#'   color vectors.
#' @export
#' @examples
#' pal <- sd_pal_c("blues")
#' pal(10)
sd_pal_c <- function(palette = "default", reverse = FALSE, ...) {
  
  # check arguments
  if (!palette %in% c("default", "blues", "grays")) stop('palette should be "default", "grays" or "blues".')
  if (!is.logical(reverse) & !is.numeric(reverse)) stop('reverse should be logical or numeric')
  
  # define palette styles
  sd_palettes <- list(
    `default`    = sd_colors(5, 1, 3),
    `blues`      = sd_colors(8:11),
    `grays`      = sd_colors(13, 15, 18)
  )
  
  # retrieve color set as unnamed vector
  pal <- sd_palettes[[palette]]
  pal <- unname(pal)
  
  # check reverse argument
  if (reverse) pal <- rev(pal)
  
  # create a color gradient with n colors
  grDevices::colorRampPalette(pal, ...)
}

#' Continuous sd color scale
#'
#' Continuous color scale for ggplot2 using sd palettes.
#'
#' @inheritParams sd_pal_c
#' @param ... Additional arguments passed to \code{ggplot2::scale_color_gradientn()}.
#'
#' @return A ggplot2 scale object.
#' @export
#' @examples
#' \dontrun{
#' ggplot(mtcars, aes(mpg, disp, color = hp)) +
#'   geom_point() +
#'   scale_color_sd_c()
#' }
scale_color_sd_c <- function(palette = "default", reverse = FALSE, ...) {
  
  # check function arguments
  if (!palette %in% c("default", "blues", "grays")) stop('palette should be "default", "grays" or "blues".')
  if (!is.logical(reverse) & !is.numeric(reverse)) stop('reverse should be logical or numeric.')
  
  # apply color set to ggplot's gradientn scale
  pal <- sd_pal_c(palette = palette, reverse = reverse)
  ggplot2::scale_color_gradientn(colors = pal(256), ...)
}

#' Continuous sd fill scale
#'
#' Continuous fill scale for ggplot2 using sd palettes.
#'
#' @inheritParams sd_pal_c
#' @param ... Additional arguments passed to \code{ggplot2::scale_fill_gradientn()}.
#'
#' @return A ggplot2 scale object.
#' @export
#' @examples
#' \dontrun{
#' ggplot(mtcars, aes(mpg, wt, fill = hp)) +
#'   geom_point(shape = 21, size = 5) +
#'   scale_fill_sd_c()
#' }
scale_fill_sd_c <- function(palette = "default", reverse = FALSE, ...) {
  
  # check function arguments
  if (!palette %in% c("default", "blues", "grays")) stop('palette should be "default", "grays" or "blues".')
  if (!is.logical(reverse) & !is.numeric(reverse)) stop('reverse should be logical or numeric.')
  
  # apply color set to ggplot's gradientn scale
  pal <- sd_pal_c(palette = palette, reverse = reverse)
  ggplot2::scale_fill_gradientn(colours = pal(256), ...)
}
