#' Replicate Figure 28.6 varying carryover effect c
#'
#' @import graphics
#'
#' @param winter.lim Dataframe from runFAC_multi() based on winter habitat limitation scenario defined by Runge and Mara (2004)
#' @param intermediate Dataframe for "Intermediate" limitaiton scenario
#' @param summer.lim Dataframe for Summer limitation scenario
#'
#' @export
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

