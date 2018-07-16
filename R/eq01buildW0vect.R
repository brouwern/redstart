#' Equation 1: Set up current population vector (W0)
#'
#' Create a vector representing population structure at the beginning of winter (W0) after
#' competition for territories has occured but before winter mortality.
#'
#' During 1st iteration (i=1) the population status taken from the runFAC() function call.
#' W0 is then updated during every iteration by taking values of \code{W.mg} , \code{W.mp},
#' etc that were calculated in equation 27 at the end of the last iteration (i-1)
#'
#' Formalizing this into an equation may not be strictly necessary but helps with aliginging
#' how the model is implemented with how it is described in the original Rung and Marra paper.
#'
#' @param W.mg Number of males (.m_) wintering in good (._g) habitat.  Scalar
#' @param W.mp Number of males (.m_) wintering in poor (._p) habitat.  Scalar
#' @param W.fg Number of females (.f_) wintering in good (._g) habitat. Scalar
#' @param W.fp Number of males (.f_) wintering in poor (._p) habitat. Scalar
#'
#' @return A vector containing the status of the population at the beginning of winter: \code{W.mg} , \code{W.mp} , \code{W.fg} , and \code{W.fp}
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal
#'       interactions in the population dynamics of migratory birds.
#'       In Greenberg, R and PP Marra, eds.  Birds of two worlds.
#'       Johns Hopkins University Press, Baltimore.
#'
#' @export


eq01buildW0vect <- function(W.mg, W.mp,
                            W.fg, W.fp){

  W0 <- c(W.mg, W.mp, W.fg, W.fp)
  names(W0) <- c("mg","mp","fg","fp")

  return(W0)
}
