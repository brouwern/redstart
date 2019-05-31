#' Plot time series of a single run of a FAC model
#'
#' @export
#'


plot_runFAC <- function(out.df,y1 = "tot.W",y2="tot.B", ...){
  graphics::par(mfrow = c(1,3),mar = c(3,3.2,2,0))
  ## Panel 1: TOtal winter vs. Total breeding
  graphics::plot(out.df[,y1] ~ out.df$t,
                 xlab = "",
                 ylab = "",
       main = "Total population size")
  mtext(text = "Iteration",side = 1,line = 2.1)
  mtext(text = "Total seasonal population size",side = 2,line = 2.1)
  graphics::points(out.df[,y2] ~ out.df$t,  #why *2?
         col = 2)
  legend("bottomright",legend = c(y1,y2),col = c(1,2), pch = 1)



  ## Panel 2: population growth rate
  graphics::plot(out.df$lambda ~ out.df$t,
                 xlab = "",
                 ylab = "",
       main = "Population growth",
       ylim = c(1,max(out.df$lambda , na.rm = T)))
  mtext(text = "Iteration",side = 1,line = 2.1)
  mtext(text = "Summer Population growth (lambda)",side = 2,line = 1.9)
  abline(h = 1, col = 2)

  ## Panel 3: winter populatison
  graphics::plot(out.df[,"W.mg"] ~ out.df$t,
                 xlab = "",
                 ylab = "",
                 main = "Winter populations",
                 ylim = c(0,1000))
  mtext(text = "Iteration",side = 1,line = 2.1)
  mtext(text = "Winter population structure",side = 2,line = 1.9)
  graphics::points(out.df[,"W.mp"] ~ out.df$t,  #why *2?
                   col = 2, pch = 2)
  graphics::points(out.df[,"W.fg"] ~ out.df$t,  #why *2?
                   col = 3, pch = 3)
  graphics::points(out.df[,"W.fp"] ~ out.df$t,  #why *2?
                   col = 4, pch = 4)
  abline(h = param_set()$K.wg/2)
  abline(h = param_set()$K.wg)
  abline(v = 60)

  legend("bottomright",legend = c("W.mg","W.mp","W.fg","W.fp"),
         col = c(1:4), pch = c(1:4))

}
