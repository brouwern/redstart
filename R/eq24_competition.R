#'Equation 24: competition for winter habitats
#'
#' @param A.i.j xxx
#' @param y.i xxx
#' @param K.wg.open.j xxx
#' @param ... xxx
#'
#' @return A.i.settled.raw.j
#'
#' @export

eq24_competition <- function(A.i.j,
                      y.i,
                      K.wg.open.j,
                      ...){
  A.i.settled.raw.j <- (A.i.j*y.i)/sum((A.i.j*y.i))*K.wg.open.j

  return(A.i.settled.raw.j)
}
