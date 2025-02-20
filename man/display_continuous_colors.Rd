% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colors.R
\name{display_continuous_colors}
\alias{display_continuous_colors}
\title{Display Continuous Color Gradient}
\usage{
display_continuous_colors(...)
}
\arguments{
\item{...}{Character strings specifying color names from the Shubham theme.
Must be valid color names from shubham_cols.}
}
\value{
Invisibly returns NULL. The function is called for its side effect
of creating a plot.
}
\description{
Creates a visual display of a continuous color gradient between specified colors
from the Shubham theme.
}
\details{
The function creates a horizontal color gradient between the specified colors.
It interpolates between the colors to create a smooth transition and displays
the color names at their respective positions along the gradient.

The gradient is created using colorRampPalette for smooth color transitions.
Color names are displayed above their starting positions in the gradient.
}
\examples{
# Create gradient between three colors
display_continuous_colors("navy", "green", "yellow")

# Create gradient between two colors
display_continuous_colors("navy", "yellow")

}
