#' Equation 18: R vector - build vector of reproduction/fecunity values
#'
#' fx.make.R.all.eq18()
#' Fx.make.P.matrix.eq18()
#' LOAD EQUATION 18b
#'
#' @details
#' REPRODUCTION
#' "average fecunidty for pairs in source and sink habitat"
#' is a function of how the proportion of pairs in that
#' habitat that are good-good, good-poor etc
#'
#' These are fixed for the entire duratin of the study
#'
#' Calculations are found in Table 28.3.
#'
#' @param R.base.rate Base rate of...
#' @param R.hab.effect Effect of breeding in poor habitat; in table 28.3 base fec is 1.8, while in poor habitat it is 0.9
#' @param co strength of carry over effect
#'
#' @return R.all All reproductive values (?)
#'
#' @export



eq18buildRmat <- function(R.base.rate,
                  R.hab.effect,
                  co){

  # Calculate fecundities
  R.cgg <- R.base.rate
  R.cgp <- R.base.rate/co
  R.cpg <- R.base.rate/co
  R.cpp <- R.base.rate/co^2
  R.kgg <- R.base.rate*R.hab.effect
  R.kgp <- R.base.rate*R.hab.effect/co
  R.kpg <- R.base.rate*R.hab.effect/co
  R.kpp <- R.base.rate*R.hab.effect/co^2

  R.all <- c(R.cgg, R.cgp, R.cpg, R.cpp,
             R.kgg, R.kgp, R.kpg, R.kpp)

  return(R.all)
}

