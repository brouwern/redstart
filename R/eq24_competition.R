# Implement equation 24: competition
eq24_competition <- function(A.i.active.j = A.i.active.j,
                      y.i = y.i,
                      K.wg.j = K.wg.j){
  A.i.settled.j.raw <- (A.i.active.j*y.i)/sum((A.i.active.j*y.i))*K.wg.j

  return(A.i.settled.j.raw)
}
