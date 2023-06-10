#' Returns a plot with the background doodle
#'
#' @param ground Choose between grass, sand, or dirt ground
#' @param sky Choose between day and night for the sky.
#'
#' @return A background doodle
#'
#' @import ggplot2
#'
#' @export
build_background <- function(ground = "grass", sky = "day"){
  color_ground <- ifelse(ground == "grass", "green4",
  ifelse(ground == "sand", "khaki1",
  ifelse(ground == "dirt", "tan4", "green4")))

  sky_color <- ifelse(sky == "day", "skyblue",
                      ifelse(sky == "night", "darkblue", "skyblue"))
  plot <-
    ggplot2::ggplot() +
    ggplot2::geom_rect(ggplot2::aes(xmin = -2, xmax = 2, ymin = -2, ymax = 0), fill = color_ground) +
    ggplot2::geom_rect(ggplot2::aes(xmin = -2, xmax = 2, ymin = 0, ymax = 2), fill = sky_color) +
    ggplot2::coord_equal(xlim = c(-1, 1), ylim = c(-1, 1)) +
    ggplot2::theme_void()
  plot
}
