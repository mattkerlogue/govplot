
#' @title Create a simple column chart
#'
#' @description A function for creating a simple column chart
#'
#' @param data A data frame for plotting
#' @param x The variable for the x-axis
#' @param y The variable for the y-axis
#' @param series Optional. The variable denoting the series
#' @param position How the series are arranged, default is "dodge", can be
#' any \link[ggplot2]{ggplot} position, the main alternative is "stack".
#'
#' @return A ggplot object
#' @export
#'
govcolumn <- function(data, x, y, series = NULL, position = "dodge") {

  x <- enquo(x)
  y <- enquo(y)
  series <- enquo(series)

  data <- data %>% mutate(!!series := forcats::as_factor(!!series))

  ggplot(data) + geom_col(aes(x = !!x, y = !!y, fill = !!series),
                          position = position)

}