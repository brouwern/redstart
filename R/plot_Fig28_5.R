#' Replicate line graph Figure 28.5 varying male dominance parameter gamma
#'
#' This varies competition for high quality winter habitat
#'
#' @param dat.winter.lim Dataframe from running a winter-limited FAC
#' @param dat.intermediate Dataframe representing an intermediate case between winter and summer limitation
#' @param dat.summer.lim Dataframe from running a summer-limited FAC
#' @param drain Should drain males be included in plots.
#'
#' @import graphics
#'
#' @export


plot_Fig28_5 <- function(dat.winter.lim,
                         dat.intermediate,
                         dat.summer.lim,
                         drain = T){
  par(mfrow = c(1,1))


  #Plot withdrain males
  if(drain == T){
    plot(sex.ratio ~ gamma.i , data = dat.winter.lim, type = "l",
         lty = 1,col = 1,ylim = c(0.98,1.12))
    points(sex.ratio ~ gamma.i , data = dat.intermediate, type = "l",
           lty = 2, col = 3)
    points(sex.ratio ~ gamma.i , data = dat.summer.lim, type = "l",
           lty = 3,col = 4)
  }


  #Plot with0 drain males
  if(drain == F){
    plot(sex.ratio.no.d ~ gamma.i , data = dat.winter.lim, type = "l",
         lty = 1,col = 1,ylim = c(0.98,1.12))
    points(sex.ratio.no.d ~ gamma.i , data = dat.intermediate, type = "l",
           lty = 2, col = 3)
    points(sex.ratio.no.d ~ gamma.i , data = dat.summer.lim, type = "l",
           lty = 3,col = 4)
  }


  text("Winter limited",
       x = 1.25,
       y = 1.08,
       pos = 4)

  text("dat.intermediate case",
       x = 3.5,
       y = 1.08,
       pos = 4)

  text("Summer limited",
       x = 3.5,
       y = 1.005,
       pos = 4)

  abline(h = 1,lty = 2,col = 2)
  abline(v = 5,lty = 2,col = 2,lwd = 5)
  legend("topright", legend = c("Winter","Intermed","Summer"),
         lty = c(1:3),col = c(1,3,4))

  title(ylab="Sex ratio",
        line=0.95, cex.lab=1)
  title(xlab="Male dominance parameter gamma",
        line=1.25,
        cex.lab=1)
}




