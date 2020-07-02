#' Replicate plot 28.4
#'
#' Functions to replicate plots that occur in Runge and Mara's Birds of Two Worlds Full Annual Cycle model
#'
#' @import graphics
#'
#' @param runFAC.multi Output of runFAC_multi
#' @param plot.debug.lines Should reference lines be plotted on graph
#'
#' @export


plot_Fig28_4_IBM<- function(runFAC.multi,
                        plot.debug.lines = T){

  #set graphical parameters
  par(mfrow = c(2,1),
      mai = c(0.5, #bot
              0.45, #left
              0.1, #top
              0.1),
      tcl=0.5,
      mgp = c(3, 0.1, 0))

  #top panel
  graphics::plot(B.tot.IB ~ K.bc.i, data = runFAC.multi, type = "l",ylim = c(0,1000),
       xlab = "",
       ylab = "")
  graphics::points(B.tot.IB-B.md.IB ~ K.bc.i, data = runFAC.multi, type = "l")
  graphics::points(B.tot.IB-B.md.IB-B.mk.IB-B.fk.IB ~ K.bc.i, data = runFAC.multi, type = "l")

  #vert line in original pub
  graphics::abline(v = 265)
  graphics::abline(v = 465)
  graphics::abline(v = 490)


  #summer
  #hort ref lines for debugging
  if(plot.debug.lines == T){
    abline(v = 450,col = 2,lty=2)
    abline(h = 530,col = 2,lty=2)
    abline(h = 655,col = 2,lty=2)
    abline(h = 965,col = 2,lty=2)
    abline(h = 935,col = 2,lty=2)

    #ref 1:1 line for debugginG
    #reported population size in "source" should not exceed carrying capacity
    #of source!; (NB: carrying capacity is in terms of PAIRS)
    points(2*K.bc.i ~ K.bc.i, data = runFAC.multi, type = "l",
           xlab = "",lty = 3,col = 2,
           ylab = "")


  }


  title(ylab="Breeding N",
        line=0.95, cex.lab=1)
  text(x = 325,y =400, label ="Source",pos = 1)

  #bottom panel
  plot(c(W.mg+W.mp+W.fg+W.fp) ~ K.bc.i,
       ylab = "",xlab = "",
       data = runFAC.multi, type = "l")
  points(c(W.mg+W.fg) ~ K.bc.i, data = runFAC.multi, type = "l")
  abline(v = 265)
  abline(v = 450)
  abline(v = 465)
  abline(v = 490)

  #winter
  #hortline
  if(plot.debug.lines == T){
    abline(h= 900,col = 2)#264.98856	896.9072
    arrows(0,0,264.98856,896.9072,col = 2,length = 0)
    arrows(265,897,  464,	1386.5979,col=3,length = 0)
    # 464.75974	1391.7526

    abline(h= 1355.67,col = 2)# # 490.1602	1355.67  599.3135	1350.5155
  }


  title(ylab="Winter N",
        line=0.95, cex.lab=1)
  title(xlab="Breeding ground capacity (pairs)",
        line=1.25,
        cex.lab=1)
  text(x = 355,y =475, label ="Good",pos = 1)
  text(x = 550,y =1250, label ="Poor",pos = 1)

}
