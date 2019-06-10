#' Equation 13: pairing in sink (.k) habitat (P.kgg)
#'
#' @details
#' good-good pairs that mate in SINK habitat
#'
#' Previously aliased as pairing.eq13.P.kgg()
#'
#' @param W2 population vector
#' @param K.bc Carrying capacity during breeding season in source habitat
#' @param K.bk Carrying capacity during breeding season in sink habitat
#' @param B.mk Males (m) allocated to sink (k)
#' @param B.fk Females allocated to sink
#' @param ... Additional parameters
#'
#' @return P.kgg Proportion of pairs in sink (k) habitat where both male and female originated from good winter habitat (gg)
#'
#' @export

eq13_Pkgg <- function(W2,
                 K.bc,
                 K.bk,
                 B.mk,
                 B.fk,
                 ...){

  #condition 3: if other conditions fails
  num   <- (min(W2["mg"], W2["fg"]) - K.bc)
  denom <-  min(B.mk, B.fk)

  P.kgg <- num/denom

  # Condition 1
  if(W2["mg"] >= (K.bc+K.bk)){
    if(W2["fg"] >= (K.bc+K.bk)){
      P.kgg <- 1}
     }


  # Condition 2
  ## 2a
  if(W2["mg"] <= K.bc){
    P.kgg<-0}

  ## 2b
  if(W2["fg"] <= K.bc) {
    P.kgg<-0}


  ## error check
  if(P.kgg > 1| P.kgg < 0){
    browser()
  }

  return(P.kgg)

}

