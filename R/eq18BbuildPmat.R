#' Equation 18: P matrix - Build 2 x 8 matrix of the proporations in each habitat combo (P.all)
#'
#' Average fecunidty for pairs in source and sink habitat"
#' is a function of how the proportion of pairs in that
#' habitat that are good-good, good-poor etc
#'
#' Previously aliased as Fx.make.P.matrix.eq18
#'
#' @param P.cgg From equation 9: the proportion of pairings in the "source" breeding habitat between male and females which both winter in the good habitat
#' @param P.cgp From equation 10: proportion males from good winter habitat paired with females from poor winter habitat
#' @param P.cpg From equation 11: the proportion of pairings in the source habitat (c) made up of males from poor winter habitat (p) and females from good winter habitat (g).
#' @param P.cpp From equation 12: Proportion of prairs between a male and female both from poor habitat
#' @param P.kgg From equation 13:Proportion of pairs in sink (k) habitat where both male and female originated from good winter habitat (gg)
#' @param P.kgp From equation 14: Proportion of pairs in sink (k) which are good-poor pairings4
#' @param P.kpg From Equation 15: Proportion of pairs in sink (k) habitat where both male originates from poor (p) and female originated from good (g) winter habitat
#' @param P.kpp From: Equation 16: Proportion of pairs in sink (k) which are poor-poor pairings
#'
#' @return P.all 2 x 8 matrix of the proporiton in each habitat
#'
#' @export


eq18BbuildPmat <- function(P.cgg = 1, P.cgp =0,P.cpg = 0, P.cpp = 0,
                           P.kgg = 1, P.kgp = 0, P.kpg = 0, P.kpp =0){
  P.all <- c(P.cgg, P.cgp,P.cpg, P.cpp,    0,     0,     0,     0,
             0,     0,     0,     0,   P.kgg, P.kgp, P.kpg, P.kpp)

  P.all <- matrix(data = P.all, nrow = 2, byrow = T)

  return(P.all)
}
