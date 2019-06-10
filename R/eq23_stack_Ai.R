#' Equation 23: Build the A matrix (A.i) (not implemented)-?????
#'
#' WHat do I mean "not implemented?" ...
#'
#' @param B2 Adults that survived breeding and fall migration
#' @param Y2 Young (offspring) that survived breeding and fall migration
#'
#' @references A.i ....
#'
#'
#' @export


eq23_stack_Ai <- function(B2,Y2){
  A.i <- c(B2,Y2)
  return(A.i)
}
