#' Equation 19: Build 2 x 2 Diagonal matrix based on minimnums of B.mc vs B.fc and B.mk vs. B.fk (min.mat)
#'
#'
#'
#' @param B.mc Abundance of males ("m") in source ("c") habitat on the breeding ground
#' @param B.fc Females in source habitat
#' @param B.mk Abundance of males in sink ("k") habitat on the breeding ground
#' @param B.fk Females in sink habitat
#'
#' @return min.mat xxx
#'
#' @example
#' eq19BbuildMinMat()
#'
#' @export



eq19BbuildMinMat <- function(B.mc= 100, B.fc = 10,
                             B.mk=10, B.fk=1){

  min.c <- min(B.mc, B.fc)
  min.k <- min(B.mk, B.fk)

  min.mat <- c(min.c, 0,
                   0, min.k)
  min.mat <- matrix(min.mat, nrow = 2, byrow = T)

  return(min.mat)
}
