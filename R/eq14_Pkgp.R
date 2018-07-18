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
#' @param ... ...
#'
#' @export

eq14_Pkgp <- function(W2,
                 K.bc,
                 K.bk,
                 B.mk,
                 B.fk, ...){

  #equation 14 last line
  P.kgp <- 0

  #equaiton 14 1st line
  if(W2["fg"] < K.bc){
    if(W2["mg"] >= (K.bc+K.bk) ){
      P.kgp <- 1}
    }


  if(W2["fg"] < K.bc){
    if(K.bc < W2["mg"]){
      if(W2["mg"] < (K.bc + K.bk)){
        P.kgp <- (unlist(W2["mg"])-K.bc)/min(B.mk, B.fk) } #can result in division by zero
    }
    }

  if(K.bc < W2["fg"]){
    if(W2["fg"] < (K.bc+K.bk)){
      if(W2["mg"] > W2["fg"]){
        P.kgp <- (min(unlist(W2["mg"]), (B.fk+K.bc)) - unlist(W2["fg"])) / min(B.mk,B.fk)
        }
    }
  }


 # check_P_division(x = P.kgp, equation_name = "eq14_Pkgp",i = i)

  ## Paper over crack
  if(is.infinite(P.kgp) == TRUE){
    #browser()
    P.kgp <- 0
  }

  return(P.kgp)

}
