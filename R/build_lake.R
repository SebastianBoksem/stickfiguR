#' Adds a lake shape to a ggplot plot
#'
#' @param background The ggplot plot to add the lake to
#' @param width The width of the lake, default is 0.65
#' @param length The length of the lake, default is 0.15
#' @param angle The angle you want the lane to be at on the plot, default is 0
#' @param shift_x The x coordinate the lake is shifted to, default is 0.50
#' @param shift_y The y coordinate the lake is shifted to, default is -0.7
#' @param color The color of the lake, default is "lightblue"
#'
#' @return The ggplot plot with the lake added
#'
#' @import ggplot2
#' @import ggforce
#'
#' @export
build_lake <- function(background, width = 0.65, length = 0.15, angle = 0, shift_x = 0.50, shift_y = -0.7, color = "lightblue") {
  background +
    ggforce::geom_ellipse(ggplot2::aes(x0 = shift_x, y0 = shift_y, a = width, b = length, angle = angle),  color = color, fill = color)
}
