#' @title \code{theme_gov} A government theme for \code{ggplot2}
#'
#' @description \code{theme_gov} Provides a theme for ggplot2 to produce
#' government style visualisations in \code{ggplot2}.
#'
#' @details Builds on the 'grammar of graphics' framework implement in
#' ggplot2. Applying \code{theme_gov()} will adjust graphical parameters
#' to give a plot a feel more in line with gov.uk.
#'
#' @param base_size Integer. Sets the base size of text for the plot.
#' Defaults to \code{12}.
#' @param base_colour Character string. Sets the default colour of axes
#'  and axis labels. Must be a named R colour or hexadecimal colour code
#' (e.g. "#FF0000"). Defaults to \code{gray40}.
#' @param axes Character string. Specifies the presence or absence of axes
#' Must be one of \code{n} (no axes), \code{x} (only x axis), \code{y}
#' (only y axis), or \code{xy} (both axes shown).
#'
#' @return Will not return anything of itself, but when used in conjuntion
#' with \code{\link{ggplot}} and (e.g.) \code{\link{geom_point}} from the
#' package \code{ggplot2}, will apply styling to a plot.
#'
#' @examples
#'
#'
#' library(ggplot2)
#' library(govstyle)
#' library(dplyr)
#'
#' p <- mtcars %>%
#'    ggplot +
#'    aes(
#'    x = mpg,
#'    y = wt
#'    ) +
#'    geom_point()
#'
#' # Plot without any theme applied
#'
#' p
#'
#' # Now apply gov theme
#'
#' p +
#'  theme_gov()
#'
#' @import ggplot2
#' @export
#'


theme_gov <- function(
  base_size = 12,
  base_colour = "gray40",
  axes = "x"
) {

  if (!axes %in% c("n","x","y","xy")) {

    stop("axes must be one of 'n', 'x', 'y', or 'xy'")

  }

  ## Set x and y axis colour

  x_col = "white"
  y_col = "white"

  if (axes == "x") {

    x_col = base_colour
    y_col = "white"

  }

  if (axes == "y") {

    x_col = "white"
    y_col = base_colour

  }

  if (axes == "xy") {

    x_col = base_colour
    y_col = base_colour

  }

  theme(
    legend.position = "none",

    ## Adjust tick marks

    axis.ticks = ggplot2::element_blank(),
    #axis.ticks = element_line(colour = "gray40"),
    #axis.ticks.y = element_blank(),
    #axis.ticks.length = grid::unit( -2, "mm"),

    ## Adjust the axis lines

    axis.line = ggplot2::element_line(colour = base_colour),
    axis.line.x = ggplot2::element_line(colour = x_col),
    axis.line.y = ggplot2::element_line(colour = y_col),

    ## Set the overall text attributes

    text = ggplot2::element_text(
      face = "plain", colour = base_colour, size = base_size,
      hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.8
    ),
    axis.text = ggplot2::element_text(colour = base_colour),
    plot.title = ggplot2::element_text(face = "bold", hjust = 1, colour = "black", vjust = -2.5),

    ## Axis title attributes. Adjustments of

    axis.title.y = ggplot2::element_text(hjust = 1, vjust = 1),
    axis.title.x = ggplot2::element_text(hjust = 1, vjust = 0),

    ## Background attributes (currently all blank)

    panel.background = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),

    ##Adjust the margin around the plot. This is highly sensitive to plot, so
    ##probably needs to be set on a plot by plot basis.

    #plot.margin = grid::unit(c(0,5,5,-30), "mm"),

    ## Strip attributes for facet grid and facet wrap

    strip.background =   ggplot2::element_blank(),
    strip.text =         ggplot2::element_text(color = "black", face = "bold", size = base_size + 1),
    strip.text.x =       ggplot2::element_text(),
    strip.text.y =       ggplot2::element_text(angle = -90),

    complete = FALSE
  )
}