# Equation 27
#  Use of assign() here is probably inefficient/awk; put into list?

eq27_post_comp_pooling <- function(A.G.i, A.P.i){

  ### Male
  W.mg <- sum(A.G.i[c("mc","mk","md","y.mc","y.mk")])
  W.mp <- sum(A.P.i[c("mc","mk","md","y.mc","y.mk")])

  ### Female
  W.fg <- sum(A.G.i[c("fc","fk",     "y.fc","y.fk")])
  W.fp <- sum(A.P.i[c("fc","fk",     "y.fc","y.fk")])

  W.list <- list(W.mg= W.mg, W.mp = W.mp,
                 W.fg = W.fg,W.fp = W.fp)

  return(W.list)
}


