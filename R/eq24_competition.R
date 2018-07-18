#' Implement equation 24: competition
#'
#' @export

eq24_competition <- function(A.i.j,
                      y.i,
                      K.wg.open.j){
  A.i.settled.raw.j <- (A.i.j*y.i)/sum((A.i.j*y.i))*K.wg.open.j

  return(A.i.settled.raw.j)
}
