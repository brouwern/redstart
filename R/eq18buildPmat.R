
#' Equation 18: Build 2 x 8 matrix of the proporations in each habitat combo
#'
#' Average fecunidty for pairs in source and sink habitat"
#' is a function of how the proportion of pairs in that
#' habitat that are good-good, good-poor etc
#'
#' Previously aliased as Fx.make.P.matrix.eq18
#'
#' @param P.cgg proportion ... Calculated by equation 9
#' @param P.cgp proportion ... Calcualted by equation 10
#' @param P.cpg proportion ... Calcualted by equation 11 (?)
#' @param P.cpp proportion ... Calcluated by subtraction using equation 12
#' @param P.kgg proportion ... Calcualted by eqation 13
#' @param P.kgp proportion ... Calcualted by eqation 14
#' @param P.kpg proportion ... Calcualted by eqation 15
#' @param P.kpp proportion ... Calcualted by eqation 16
#'
#' @return P.all 2 x 8 matrix of the proporiton in each habitat
#'
#' @export


eq18buildPmat <- function(P.cgg, P.cgp,P.cpg, P.cpp,
                                           P.kgg, P.kgp, P.kpg, P.kpp){
  P.all <- c(P.cgg, P.cgp,P.cpg, P.cpp,    0,     0,     0,     0,
             0,     0,     0,     0,   P.kgg, P.kgp, P.kpg, P.kpp)

  P.all <- matrix(data = P.all, nrow = 2, byrow = T)

  return(P.all)
}
