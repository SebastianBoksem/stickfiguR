#' Returns a plot with a house added
#'
#' @param background The previous background you have created.
#' @param scale The scale size of the stick figure, default is .25.
#' @param shift_x If you would want to shift the stick figure left or right, the minimum and maximum is -1 and 1.
#' @param shift_y If you would want to shift the stick figure up or down, the minimum and maximum is -1 and 1.
#' @param color the color of the house you would like.
#'
#' @return a house
#'
#' @import ggplot2
#'
#' @export
build_house <- function(background, scale = 0.25, shift_x = 0, shift_y = 0, color = "red"){
  scale <- scale
  shift_x <- shift_x
  shift_y <- shift_y
  background +
    ggplot2::geom_rect(ggplot2::aes(xmin = -.5* scale + shift_x, xmax = .5* scale + shift_x, ymin = -.5* scale + shift_y, ymax = .5* scale + shift_y), fill = color) +
    ggplot2::geom_polygon(ggplot2::aes(x = c(-.5* scale + shift_x, 0* scale + shift_x, .5* scale + shift_x) , y = c(.5* scale + shift_y, 1* scale + shift_y, .5* scale + shift_y)), fill = color) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .25* scale + shift_x, xmax = .45* scale + shift_x, ymin = -.45* scale + shift_y, ymax= .25* scale + shift_y), fill = "grey") +
    ggplot2::theme_void()
}


