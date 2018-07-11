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
#'
#' @export


eq15 <- function(W2,
                 K.bc,
                 K.bk,
                 B.mk,
                 B.fk){

  if(  W2["mg"] < K.bc)
    if(W2["fg"] >= (K.bc + K.bk))
    {r <- 1 }

  if(W2["mg"] < K.bc)
    if( K.bc < W2["fg"])
      if (W2["fg"] < (K.bc+K.bk))
      { r<- (W2["fg"]-K.bc)/min(B.mk, B.fk)}

  if(K.bc < W2["mg"])
    if(W2["mg"] < (K.bc + K.bk))
      if(W2["fg"] > W2["mg"])
      { r <- (min(W2["fg"], (B.mk+K.bc))-W2["mg"])/min(B.mk,B.fk) }

  r <- 0

  return(ifelse(r != "NaN",r,0))
}
