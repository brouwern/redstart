#' Equation 13: pairing in sink (.k) habitat
#'
#' @details
#' good-good pairs that mate in SINK habitat
#'
#' Previously aliased as pairing.eq13.P.kgg()
#'
#' @param W2 population vector
#' @param K.bc carrying capacity...
#' @param K.bk carrying capacity...
#' @param B.mk ...
#' @param B.fk ...
#'
#' @export

eq13 <- function(W2,
                 K.bc,
                 K.bk,
                 B.mk,
                 B.fk){

  #condition 3: if other conditions fails
  num <- (min(W2["mg"], W2["fg"]) - K.bc)
  denom <-  min(B.mk, B.fk)

  r <- num/denom

  # Condition 1
  if(W2["mg"] >= (K.bc+K.bk)){
    if(W2["fg"] >= (K.bc+K.bk)){
      r <- 1}
     }


  # Condition 2
  ## 2a
  if(W2["mg"] <= K.bc){
    r<-0}

  ## 2b
  if(W2["fg"] <= K.bc) {
    r<-0}


  #error message
      if(is.nan(r) == TRUE){
        message("ERROR IN EQUATION 13: NaN!!")
      }


  r <- ifelse(is.nan(r) == TRUE, 0, r)

  return(r)

}

