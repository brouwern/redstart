#' Equation 14: Proportion of pairs in sink habitat (k) good-poor pairs (P.kgp)
#'
#'
#' @param W2 Population vector
#' @param K.bc Carrying capacity (K) of source (high quality) breeding habitat.
#' @param K.bk Carrying capacity (K) of sink (low quality) breeding habitat.
#' @param B.mk males allocated to sink
#' @param B.fk females allocated to sink
#' @param ... additional parameters
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal interactions in the population dynamics of migratory birds. In Greenberg, R and PP Marra, eds.  Birds of two worlds. Johns Hopkins University Press, Baltimore.
#'
#' @return P.kgp Proportion of pairs in sink (k) which are good-poor pairings
#'
#' @examples
#' # Trivial example
#' W2. <- c(10,10,10,10)
#' names(W2.) <- c("mg","fg","mp","fp")
#' eq14_Pkgp(W2 = W2., K.bc = 0, K.bk = 10, B.mk = 5, B.fk =5)
#'
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
          warning()}

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
        ## P.kgp <- ifelse(numerator> denominator,1,P.kgp)

        ### if the denominator is ~0, then P = 0
        ## P.kgp <- ifelse(floor(denominator) == 0,0,P.kgp)




        }
    }
  }


  if(P.kgp > 1 |  P.kgp < 0){
    warning()
  }

  return(P.kgp)

}
