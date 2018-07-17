#' Equation 4: Allocate breeding females to Source habitat (B.fc)
#'
#' @description This function outputs \code{B.fc}, the number of females that acquire
#' high quality "source" (._c) habitat during the breeding season
#'
#' @details
#' This function compares total female population size after migration (W2.fg + W2.fp)
#' to K.bc the souce carrying capacity \code{K.bc}; if the total population of
#' females < K.bc, all females acquire habitat in the source.
#' If W2.fg + W2.fp > K.bc, then the source habitat gets filled up completely.  Any additinal
#' females will be allocated to the sink (._k) habitat in a subsequent step.
#'
#' The output of this function is used in equation 17.
#'
#' Previous alias: F.2.source.eq4.c
#'
#' Part of summer female dynamics
#'
#' @param W2 Population vector produced after winter and northward migration survival.
#' @param K.bc carrying capacity of breeding ground (.k_) source (._c) habitat
#'
#' @return Abundance B...
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal
#'       interactions in the population dynamics of migratory birds.
#'       In Greenberg, R and PP Marra, eds.  Birds of two worlds.
#'       Johns Hopkins University Press, Baltimore.
#'
#' @examples
#' # Set parameters:
#' ## Create named population W2 vector using the eq01buildW0vect()
#' W2 <- eq01buildW0vect(100,10,100,10)
#'
#' W2
#'
#' #Breeding habitat carrying capacity
#' K.bc.110 <- 110
#' K.bc.100 <- 100
#' K.bc.10  <- 10
#'
#' eq04_Bfc(W2, K.bc.110)
#' eq04_Bfc(W2, K.bc.100)
#' eq04_Bfc(W2, K.bc.10)
#'
#' @export

eq04_Bfc <- function(W2,
                     K.bc
){
  if((W2["fg"] + W2["fp"])  < K.bc){
    B.fc <- (W2["fg"] + W2["fp"])
  }else{
    B.fc <- K.bc
  }

 names(B.fc) <- "B.fc"
  return(B.fc)
}



