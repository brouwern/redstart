#' Equation 14: proportion in sink habitat, good-poor pairs
#'
#' @details
#'
#' Previously aliased as pairing.eq14.P.kgp()
#'
#' @param W2 population vector
#' @param K.bc carrying capacity ...
#' @param K.bk carrying capacity ...
#' @param B.mk ...
#' @param B.fk ...
#'
#' @export

eq14 <- function(W2,
                 K.bc,
                 K.bk,
                 B.mk,
                 B.fk){

  r <- 0

  if(W2["fg"] < K.bc)
    if(W2["mg"] >= (K.bc+K.bk) )
    {r <- 1}

  if(W2["fg"] < K.bc)
    if(K.bc < W2["mg"])
      if(W2["mg"] < (K.bc + K.bk))
      {r<-(W2["mg"]-K.bc)/min(B.mk, B.fk) }

  if(K.bc < W2["fg"])
    if(W2["fg"] < (K.bc+K.bk))
      if(W2["mg"] > W2["fg"])
      {r <- (min(W2["mg"], (B.fk+K.bc)) - W2["fg"]) / min(B.mk,B.fk)}

  if(is.nan(r) == TRUE){
    message("ERROR IN EQUATION 14: NaN!!")
  }


  return(ifelse(r != "NaN",r,0))
}
