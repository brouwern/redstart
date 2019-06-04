#' Equation 19: Build 2 x 2 Diagonal matrix based in minimumbs of B.mc vs B.fc and B.mk vs. B.fk
#'
#' Fx.make.min.mat.eq19()
#'
#' @details
#' Previously aliased as Fx.make.min.mat.eq19
#'
#' @param B.mc xxx
#' @param B.fc xxx
#' @param B.mk xxx
#' @param B.fk xxx
#'
#' @return min.mat xxx
#'
#' @export



eq19BbuildMinMat <- function(B.mc, B.fc,
                             B.mk, B.fk){

  min.c <- min(B.mc, B.fc)
  min.k <- min(B.mk, B.fk)

  min.mat <- c(min.c, 0, min.k, 0)
  min.mat <- matrix(min.mat, nrow = 2, byrow = T)

  return(min.mat)
}
