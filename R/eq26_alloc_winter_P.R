#' equation 26 Allocate to poor habitat
#'
#' @export



eq26_alloc_winter_P <- function(A.i.0,
                                A.G.i){
  #        initial - number settled in good habitat
  A.P.i <- A.i.0   -  A.G.i
  return(A.P.i)
}


