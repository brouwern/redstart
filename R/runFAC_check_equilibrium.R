#' Check a model being run for equilibrium
#'
#' @param lamb.B.mc.mean Mean lambda from B.mc ..
#' @param lamb.B.mc.var Variance of lambda from B.mc ...
#' @param i iteration
#' @param at.eq At equilibrium
#' @param verbose Verbose output?
#' @param eq.tol Equilibrium tolerance level (higher is more conservative)
#' @param ... Additional arguements
#'
#' @references at.eq .... Logical TRUE ?
#'
#' @export

runFAC_check_equilibrium <- function(lamb.B.mc.mean,
                                     lamb.B.mc.var,
                                     i,
                                     at.eq = FALSE,
                                     verbose = TRUE,
                                     eq.tol = 5,
                                     ...){


 # out.df$lambda.B.mc.i[i]
  if(round(lamb.B.mc.mean,eq.tol) == 1 &
     round(lamb.B.mc.var, eq.tol) == 0){
    at.eq <- TRUE
  }



  if(at.eq == TRUE & verbose == TRUE){

    message("\nModel at equilibrium after ",i," iterations")

  }

  return(at.eq)
}



