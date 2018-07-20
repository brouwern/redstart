#' equation 26 Allocate to poor habitat
#'
#' @export



eq26_alloc_winter_P <- function(A.i.0,
                                A.i.G){
  #        initial - number settled in good habitat
  A.i.P <- A.i.0   -  A.i.G
  return(A.i.P)
}


