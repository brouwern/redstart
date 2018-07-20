#' Equation 27
#' Use of assign() here is probably inefficient/awk; put into list?
#'
#' @export

eq27_post_comp_pooling <- function(A.i.G, A.i.P){

  ### Male
  W.mg <- sum(A.i.G[c("mc","mk","md","y.mc","y.mk")])
  W.mp <- sum(A.i.P[c("mc","mk","md","y.mc","y.mk")])

  ### Female
  W.fg <- sum(A.i.G[c("fc","fk",     "y.fc","y.fk")])
  W.fp <- sum(A.i.P[c("fc","fk",     "y.fc","y.fk")])

  W.list <- list(W.mg= W.mg, W.mp = W.mp,
                 W.fg = W.fg,W.fp = W.fp)

  return(W.list)
}


