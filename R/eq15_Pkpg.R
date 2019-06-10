#' Equation 15: Proportion in sink poor-poor (P.kpg)
#'
#'
#' @details
#'
#' Aliased as pairing.eq15.P.kpg()
#'
#' @param W2 Popualtion vector
#' @param K.bc carrying capacity
#' @param K.bk carrying capacity
#' @param B.mk males allocated to sink
#' @param B.fk females allocated to sink
#'
#' @return P.kpg Proportion of pairs in sink (k) habitat where both male originates from poor (p) and female originated from good (g) winter habitat
#'
#' @export


eq15_Pkpg <- function(W2,
                 K.bc,
                 K.bk,
                 B.mk,
                 B.fk){

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
        denominator <- min(B.mk,B.fk)
        P.kpg <-  numerator/denominator}
    }
  }


  P.xxx <- P.kpg
  if(P.xxx > 1){
    browser()
  }

  if(P.xxx < 0){
    browser()
  }

  if(is.infinite(P.xxx) == TRUE){
    browser()
  }

  if(is.na(P.xxx) == TRUE){
    browser()
  }

  if(is.nan(P.xxx) == TRUE){
    browser()
  }

  return(P.kpg)
}
