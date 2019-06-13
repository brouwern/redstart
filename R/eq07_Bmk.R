#' Equation 7: Allocate breeding males to sink habitat (B.mk)
#'
#' This function output B.mk, the number of breeding (B.) males (.m) in
#' sink habitat (.k).
#'
#' Summer Male (M) Dynamics - MALES that acquire sink habitat "the number of males in sink habitat is limited by being able to find a mate, hence B.fk rather than K.bk" That is, if the female population is less than K.bk for some reason then males remain unpaired;better to be a floater than an unpaired male? but floaters have lower survival...
#'
#' How the function works:
#' 1) If W2.mg + W2.mp < K.bc, then 0 males go to sink (eg, total males less than source size then all avaiable males can go to source it appears as if these males will go there even if they remain unpaired)
#'
#' 2)If K.bc <= (W2.mg + W2.mp) AND
#'   If W2.mg + W2.mp < K.bc+B.fk
#'   Then W2.mg + W2.mp - K.bc males go to sink
#'   (That is, if total males greater than or equal to source carrying capacity AND total males are less than source K and number of females in sin.k. )THEN the numbe rof males that go to sin.k. equals total males minus those that went to source
#' 3) If total males is GREATER than the number of females (K.bc + B.fk) then the number of males that go to the sink is equal to the number of females in the sink, B.fk total females = K.bc + B.fk b/c...
#'
#' @param W2 population vector
#' @param K.bc source carrying capacity
#' @param B.fk females allocated to sink
#'
#' @return B.mk, the number of breeding males in sink (.k) habitat
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal interactions in the population dynamics of migratory birds. In Greenberg, R and PP Marra, eds.  Birds of two worlds. Johns Hopkins University Press, Baltimore.
#'
#' @export

eq07_Bmk <- function(W2,
                     K.bc,
                     B.fk){

  if( (unlist(W2["mg"]) + unlist(W2["mp"])) < K.bc){
    B.mk <- 0
     }

  #K.bc = sink carrying capacity;
  if(K.bc <= (unlist(W2["mg"]) + unlist(W2["mp"])) ){
    if( (unlist(W2["mg"]) + unlist(W2["mp"])) < (K.bc+unlist(B.fk)) ){
      B.mk <- unlist(W2["mg"]) + unlist(W2["mp"]) - K.bc
      }
  }


  if( (unlist(W2["mg"]) + unlist(W2["mp"])) >= (K.bc + unlist(B.fk)) ){
    B.mk <- unlist(B.fk)
    }

  return(B.mk)

 }


