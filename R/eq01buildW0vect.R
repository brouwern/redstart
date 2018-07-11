#' Equation 1: Set up current population vector W0
#'
#' Create a vector representing Population at beginning of winter (W0)
#'
#' During 1st iteration (i=1) this taken from function call
#' This is updated during every iteration by taking values of W.mg, W.mp
#' etc that were calculated in equation 27 at the end of the last iteration (i-1)
#'
#'
#' @param W.mg scalar
#' @param W.mp scalar
#' @param W.fg scalar
#' @param W.fp scalar
#'
#' @return A vector containing the status of the popualtion at the beginning of winter: \code{W.mg} , \code{W.mp} , \code{W.fg} , and \code{W.fp}
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

  return(W0)
}
