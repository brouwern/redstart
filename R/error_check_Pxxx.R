#' QA/QC: Check for errors in pairing equations
#'
#' @param P.xxx.name name of scalar to check
#' @param P.xxx.value value for a given P.xxx
#' @param i Current iteration of runFAC()
#' @param P.xxx.eq.name full name of equation
#' @param ... additional paramters
#'
#' @examples
#' P.xxx.names <- c("P.cgg", "P.cgp","P.cpg", "P.cpp","P.kgg", "P.kgp","P.kpg", "P.kpp")
#' P.xxx.values <- runif(min = -0.8,max = 1.2,n= length(P.xxx.names))
#' P.xxx.values[3] <- NaN
#'
#' error_check_Pxxx(P.xxx.name = P.xxx.names[3],
#'                  P.xxx.value = P.xxx.values[3])
#'
#' for(i in 1:length(P.xxx.names)){
#'    error_check_Pxxx(P.xxx.name = P.xxx.names[i],
#'                  P.xxx.value = P.xxx.values[i])}
#' @export


error_check_Pxxx <- function(P.xxx.name,
                             P.xxx.value,
                             P.xxx.eq.name = NA,
                             i = NA,
                             ...){


  #Is NaN (not a number)
  if(is.nan(P.xxx.value)==TRUE){
    message(P.xxx.name," is NaN on iteration ",i,"; equation ",P.xxx.eq.name)
  }

  #Is NA
  if(is.na(P.xxx.value)==TRUE){
    message(P.xxx.name," is NA on iteration ",i,"; equation ",P.xxx.eq.name)
  }

  #Is infiinte
  if(is.infinite(P.xxx.value)==TRUE){
    message(P.xxx.name," is inf on iteration ",i,"; equation ",P.xxx.eq.name)
  }

  #Is > 1?
  if(is.nan(P.xxx.value) == F & P.xxx.value > 1){
    message(P.xxx.name," is >1 on iteration ",i,
            "; P = ",round(P.xxx.value,2),"; equation ",P.xxx.eq.name)
  }


  #Is < 0
  if(is.nan(P.xxx.value) == F & P.xxx.value < 0){
    message(P.xxx.name," is <0 on iteration ",i,"; P = ",
            round(P.xxx.value,2),
            "; equation ",P.xxx.eq.name)
  }

}
