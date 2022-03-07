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
  plot(B.tot ~   c.i, data = winter.lim,
       type = "l",ylim = c(350,750),
       lwd = 2,
       lty = 1)
  points(B.tot ~ c.i,
         lty = 1,
         col = 1,
         lwd = 2,
         data = intermediate,
         type = "l") # carry over previous was c.i in output used here; currently c.i
  points(B.tot ~ c.i,
         data = summer.lim,
         type = "l",
         col = 1,
         lty = 1,
         lwd = 2)

  #abline(h = 500, lty = 3,col = 3)

  text(y = 510,
       x = 1.2,
       "Summer limited")

  text(y = 485,
       x = 1.5,
       "Intermediate case")

  text(y = 415,
       x = 1.8,
       "Winter limited")
}

