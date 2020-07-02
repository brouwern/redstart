#' Equation 18: Build vector of reproduction/fecundity values (R.all)
#'
#' "average fecunidty for pairs in source and sink habitat" is a function of how the proportion of pairs in that habitat that are good-good, good-poor etc.  (Runge and Marra 2004, pg TOD())
#'
#' By default these are fixed for the entire duration of the study.
#'
#' Calculations are found in Table 28.3.
#'
#' @param R.base.rate Base rate of TODO wbhat
#' @param R.hab.effect Effect of breeding in poor habitat; in table 28.3 base fec is 1.8, while in poor habitat it is 0.9, so habitat effect is 0.5
#' @param co strength of carry over effect
#'
#' @return R.all All reproductive values (TODO check)
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal interactions in the population dynamics of migratory birds. In Greenberg, R and PP Marra, eds.  Birds of two worlds. Johns Hopkins University Press, Baltimore.
#'
#' @examples
#' #Show contents of the vector
#' eq18AbuildRvec()
#'
#' @export



eq18AbuildRvec <- function(R.base.rate = 1.8,
                  R.hab.effect = 0.5,
                  co = 1){

  # Calculate fecundities
  R.cgg <- R.base.rate
  R.cgp <- R.base.rate/co
  R.cpg <- R.base.rate/co
  R.cpp <- R.base.rate/(co^2)

  R.kgg <- (R.base.rate*R.hab.effect)
  R.kgp <- (R.base.rate*R.hab.effect)/co
  R.kpg <- (R.base.rate*R.hab.effect)/co
  R.kpp <- (R.base.rate*R.hab.effect)/(co^2)

  R.all <- c(R.cgg, R.cgp, R.cpg, R.cpp,
             R.kgg, R.kgp, R.kpg, R.kpp)

  return(R.all)
}

