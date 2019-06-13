#' Equation 24: Make relative competitivness parameters for winter habitat acquition
#'
#' Competitive ability depends on "an intrsince age-,sex- and condition(habitat)-specific competitive factor gamma and the number of birds in each class" (Runge and Mara, 2004; page 380, column 2).  THis function calculates gammas for each class of birds based on gamma.base provided in function call.  Currently competitiveness for winter habitat is primarily determined by age and the habitat a bird originated (source vs. sink).  Changing gamma.base decreases the competitiveness of females relative to males.
#'
#' Note: breeding K is based on pairs,winter K is based on individuals.
#'
#'
#'
#' @param gamma.base Base competition value of females relative to males.   Higher values make females LESS competitive.
#'
#' @return gamma.i Vector of competiveness values for each class of birds
#'
#' @examples
#'
#' # Default relative competitiveness is that males = females
#' gamma.1 <- eq24_make_gamma_vec()
#'
#' # Make females more competitive than males
#' gamma.1.2 <- eq24_make_gamma_vec(gamma.base = 1.2)
#'
#' data.frame(gamma.1, gamma.1.2)
#'
#' @export


eq24_make_gamma_vec <- function(gamma.base = 2){
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

