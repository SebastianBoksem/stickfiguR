#' Adds a sun to the background
#'
#' @param background The previous background you have created
#' @param size The size of the sun, default is 0.25
#' @param shift_x The horizontal shift of the sun, default is 0
#' @param shift_y The vertical shift of the sun, default is 0
#'
#' @return A background with the sun added
#'
#' @import ggplot2
#' @import ggforce
#'
#' @export
add_sun <- function(background, size = 0.25, shift_x = 0.75, shift_y = 0.75) {
  scale <- size
  radius <- size / 2
  shift_x <- shift_x
  shift_y <- shift_y

  background +
    ggforce::geom_circle(ggplot2::aes(x0 = shift_x, y0 =shift_y, r = radius), color = "#fdd835", fill = "#fdd835")

}

#' Adds a moon to the background
#'
#' @param background The previous background you have created
#' @param size The size of the moon, default is 0.25
#' @param shift_x The horizontal shift of the moon, default is 0
#' @param shift_y The vertical shift of the moon, default is 0
#'
#' @return A background with the moon added
#'
#' @import ggplot2
#'
#' @export
add_moon <- function(background, size = 0.25, shift_x = 0.75, shift_y = 0.75) {
  radius <- size / 2
  shift_x <- shift_x
  shift_y <- shift_y

  background +
    ggforce::geom_circle(ggplot2::aes(x0 = shift_x, y0 =shift_y, r = radius), color = "#F2F3F5", fill = "#F2F3F5")

}

#' Adds sun or moon to the background
#' @param type sun or moon
#' @param background The previous background you have created
#' @param size The size of the sun or moon, default is 0.25
#' @param shift_x The horizontal shift of the sun or moon, default is 0
#' @param shift_y The vertical shift of the sun or moon, default is 0
#'
#' @return A background with the sun or moon added
#'
#' @import ggplot2
#'
#' @export

build_sun_or_moon <- function(type = "sun", background, size = 0.25, shift_x = 0.75, shift_y = 0.75) {
  if (type == "sun") {
    return(add_sun(background, size = size, shift_x = shift_x, shift_y = shift_y))
  } else if (type == "moon") {
    return(add_moon(background, size = size, shift_x = shift_x, shift_y = shift_y))
  } else {
    stop("Invalid type specified. Please choose 'sun' or 'moon'.")
  }
}


