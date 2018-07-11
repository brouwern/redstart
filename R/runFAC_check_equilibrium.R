
runFAC_check_equilibrium <- function(out.df, i, tol.2,at.eq, ...){

  #run regression through last 10 iterations
  # to determine if population stable

  ## run model
  mod <- stats::lm(out.df$W.fg[c(i-10):i] ~
                     c(1:length(out.df$W.fg[c(i-10):i])))
  ## get slope
  coef.out <- stats::coef(mod)[2]

  local.slope <- round(coef.out, tol.2)


  if(local.slope == 0){
    #browser()
    message("\nModel at equilibrium after ",i," iterations")
    at.eq <- TRUE
  }

  return(at.eq)
}

