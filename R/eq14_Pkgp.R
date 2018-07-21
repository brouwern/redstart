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
                 B.fk,
                 i = NA){

  #equation 14 last line
  P.kgp <- 0

  #equaiton 14 1st line
  if(W2["fg"] < K.bc){
    if(W2["mg"] >= (K.bc+K.bk) ){
      P.kgp <- 1}
    }


  if(W2["fg"] < K.bc){    #not enough females in source to fill it, so all femalse in sink will be poor
    if(K.bc < W2["mg"]){  #more good males than available source territories, so there are males in sink
      if(W2["mg"] < (K.bc + K.bk)){
        numerator <- unlist(W2["mg"])-K.bc #number of good males settled in source
        denominator <- min(B.mk, B.fk)
        P.kgp <- numerator/denominator  #can result in division by zero

        ## Error control
        ### If numerator > denominator, then P = 1 (!!! is this correct?)
        P.kgp <- ifelse(numerator> denominator,1,P.kgp)

        ### if the denominator is ~0, then P = 0
        P.kgp <- ifelse(floor(denominator) == 0,0,P.kgp)

        #error check
        if(P.kgp > 1){
          browser()}

        }


       }
      }


  if(K.bc < W2["fg"]){
    if(W2["fg"] < (K.bc+K.bk)){
      if(W2["mg"] > W2["fg"]){
        numerator <- min(unlist(W2["mg"]), (B.fk+K.bc)) - unlist(W2["fg"])
        denominator <- min(B.mk, B.fk)
        P.kgp <- numerator/ denominator

        ## Error control
        ### If numerator > denominator, then P = 1 (!!! is this correct?)
        P.kgp <- ifelse(numerator> denominator,1,P.kgp)

        ### if the denominator is ~0, then P = 0
        P.kgp <- ifelse(floor(denominator) == 0,0,P.kgp)

        #error check
        if(P.kgp > 1){
          browser()
        }


        }
    }
  }


  return(P.kgp)

}
