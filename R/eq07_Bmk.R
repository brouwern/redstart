#' Equation 7: Allocate breeding males to sink habitat (B.mk)
#'
#' This function outputs \code{B.mk}, the number of breeding (B.) males (.m) in sink habitat (.k).
#'
#' The Runge and Marra (2004) indicate that "the number of males in sink habitat is limited by being able to find a mate, hence B.fk rather than K.bk." (pg TODO()). That is, if the female population is less than \code{K.bk} for some reason then males remain unpaired; it is therefore better to be a floater in the drain than an unpaired male.  TODO() Check the implications of this
#'
#' How the function works:
#' 1) IF \code{W2.mg + W2.mp < K.bc}
#' THEN 0 males go to sink
#' That is, if the total number of males is less than source size then all avaiable males can go to source; it appears as if these males will go there even if they remain unpaired.
#'
#' 2) IF \code{K.bc <= (W2.mg + W2.mp)}
#' AND IF \code{W2.mg + W2.mp < K.bc+B.fk}
#' THEN \code{W2.mg + W2.mp - K.bc} males go to sink
#'   (That is, IF total males greater than or equal to source carrying capacity AND total males are less than source K and number of females in sin.k. ) THEN the number of males that go to sin.k. equals total males minus those that went to source.
#'
#' 3) IF total males is GREATER than the number of females (\code{K.bc + B.fk})
#' THEN the number of males that go to the sink is equal to the number of females in the sink, \code{B.fk}. The total number of females = K.bc + B.fk because ... TOD():...
#'
#' @param W2 population vector
#' @param K.bc source carrying capacity
#' @param B.fk females allocated to sink
#'
#' @return B.mk, the number of breeding males in sink (.k) habitat
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal interactions in the population dynamics of migratory birds. In Greenberg, R and PP Marra, eds.  Birds of two worlds. Johns Hopkins University Press, Baltimore.
#'
#' @examples
#' W2. <- c(10,10,10,10)
#' names(W2.) <- c("mg","fg","mp","fp")
#' eq07_Bmk(W2 = W2., K.bc = 0, B.fk =5)
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


