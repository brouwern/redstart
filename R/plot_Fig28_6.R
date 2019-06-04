#'  Make plot 28.6 - varying carry over effect c
#'
#' @import graphics
#'
#' @param winter.lim xxx
#' @param intermediate xxx
#' @param summer.lim xxx
#'
#' @export
#'
#'
#'

plot_Fig28_6 <- function(winter.lim,
                         intermediate,
                         summer.lim){
  plot(B.tot ~   co.i, data = winter.lim,
       type = "l",ylim = c(350,750))
  points(B.tot ~ co.i, lty = 2,col = 2,data = intermediate, type = "l")
  points(B.tot ~ co.i, data = summer.lim, type = "l")
  abline(h = 500, lty = 3,col = 3)

  text(y = 1.1,
       x = 510,
       "summer limited")

  text(y = 1.5,
       x = 745,
       "Intermediate case")

  text(y = 1.8,
       x = 425,
       "Intermediate case")
}

