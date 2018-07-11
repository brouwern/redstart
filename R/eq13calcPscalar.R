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
  if(W2["mg"] >= (K.bc+K.bk))
    if(W2["fg"] >= (K.bc+K.bk))
    {return(1)}
  if(W2["mg"] <= K.bc)
  {return(0)}
  if(W2["fg"] <= K.bc)
  {return(0)}

  num <- (min(W2["mg"], W2["fg"]) - K.bc)
  denom <-  min(B.mk, B.fk)
  return(ifelse(num/denom != "NaN", num/denom, 0))

}

