check_P_division <- function(x,equation_name,i,debug.it = FALSE,...){

  if(is.nan(x)==TRUE){
    message(equation_name," is NaN on iteration ",i)
  }

  if(is.na(x)==TRUE){
    message(equation_name," is NA on iteration ",i)
  }

  if(is.infinite(x)==TRUE){
    message(equation_name," is NaN on iteration ",i)
  }

  if(debug.it == TRUE){
    browser()
  }
}
