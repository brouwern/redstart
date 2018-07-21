#' Equation 15: Proportion in sink poor-poor
#'
#'
#' @details
#'
#' Aliased as pairing.eq15.P.kpg()
#'
#' @param W2 Popualtion vector
#' @param K.bc carrying capacity
#' @param K.bk carrying capacity
#' @param B.mk ...
#' @param B.fk ...
#' @param i index of loop.  for debugging
#'
#' @export


eq15_Pkpg <- function(W2,
                 K.bc,
                 K.bk,
                 B.mk,
                 B.fk,
                 i = i){

  #equation 15 last line
  P.kpg <- 0

  #equation 15 line 1
  if(  W2["mg"] < K.bc){
    if(W2["fg"] >= (K.bc + K.bk)){
      P.kpg <- 1 }
    }


  if(W2["mg"] < K.bc){
    if( K.bc < W2["fg"]){
      if (W2["fg"] < (K.bc+K.bk)){
        #browser()
        numerator <- unlist(W2["fg"])-K.bc
        denominator <- min(B.mk, B.fk)
        P.kpg <- numerator/denominator}
    }
  }


  if(K.bc < W2["mg"]){
    if(W2["mg"] < (K.bc + K.bk)){
      if(W2["fg"] > W2["mg"]){
        numerator <- min(unlist(W2["fg"]), (B.mk+K.bc))-unlist(W2["mg"])
        denominator <- numerator/min(B.mk,B.fk)
        P.kpg <-  denominator}
    }
  }



  return(P.kpg)
}
