#' Equation 24 - Competition parameters (gamma)
#'
#' Calculates gammas based on gamma.base provided in function call
#'
#' @details
#' NB: breeding K is based on pairs,
#'    winter K is based on individuals!
#' competition ability depends on "an intrsince age-,sex-
#' and condition(habitat)-specific competitive factor
#' gamma and the number of birds in each class"
#'
#'
#'    gamma represents "an instrinsic age-,sex,- and condition (habitat)-specific...factor" pg380 col 2
#'
#' previously aliased as fx.make.gamma.i.eq24
#'
#' @param gamma.base Competition
#'
#' @export


eq24_make_gamma_vec <- function(gamma.base){
  gamma.i <-  c(
    #Adult males
    1,
    0.1,  #Poor habitat have 1/10th comp ability of those from good habitat
    0.01,

    #Adult females
    1.0/gamma.base,
    0.1/gamma.base,

    #Young male
    0.20,
    0.01,

    #Young female
    0.20/gamma.base, #gamma.base
    0.01)

  return(gamma.i)
}

