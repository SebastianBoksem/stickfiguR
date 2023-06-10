#' Returns a plot with the stick figure added
#'
#' @param background The previous background you have created
#' @param scale The scale size of the stick figure, default is .25
#' @param shift_x If you would want to shift the stick figure left or right, the minimum and maximum is -1 and 1.
#' @param shift_y If you would want to shift the stick figure up or down, the minimum and maximum is -1 and 1.
#'
#' @return A stick figure
#'
#' @import ggplot2
#'
#' @export
build_stick_figure <- function(background, scale = 0.25, shift_x = 0, shift_y = 0){
  scale <- scale
  radius <- 0.5 * scale
  shift_x <- shift_x
  shift_y <- shift_y
  num_points <- 100
  theta <- seq(0, 2 * pi, length.out = num_points)
  x <- radius * cos(theta) + shift_x
  y <- radius * sin(theta) + scale + scale/2 + shift_y
  df <- data.frame(x = x, y = y)
  background +
    ggplot2::geom_segment(show.legend = FALSE,ggplot2::aes(x = -1 * scale + shift_x, y = -1 * scale + shift_y, xend = shift_x, yend = shift_y, size = 1)) +
    ggplot2::geom_segment(show.legend = FALSE,ggplot2::aes(x = 1 * scale + shift_x, y = -1 * scale + shift_y, xend = shift_x, yend = shift_y, size = 1)) +
    ggplot2::geom_segment(show.legend = FALSE,ggplot2::aes(x = shift_x, y = shift_y, xend = shift_x, yend = 1 * scale + shift_y, size = 1)) +
    ggplot2::geom_segment(show.legend = FALSE,ggplot2::aes(x = -1 * scale + shift_x, y = shift_y, xend = shift_x, yend = 1 * scale + shift_y, size = 1)) +
    ggplot2::geom_segment(show.legend = FALSE,ggplot2::aes(x = 1 * scale + shift_x, y = shift_y, xend = shift_x, yend = 1 * scale + shift_y, size = 1)) +
    ggplot2::geom_path(data = df, show.legend = FALSE, ggplot2::aes(x, y, size = 1)) +
    ggplot2::theme_void()
}

