#' Equation 6: Allocate breeding males to source habitat (B.mc)
#'
#' @details
#' Summer male (M) dynamces
#' males acquiring  source habitat
#' I believe this allows males to be holding territories
#' w/o pairing w/ a female
#'
#' Aliased as M.2.source.eq6.c()
#'
#' @param W2 Population vector produced by eq02buildW1Mat()
#' @param K.bc Carrying capacity (K) during breeding season (b) of source (c) habitat
#'
#' @return B.mc Total number of males allocated to source habitat.
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal interactions in the population dynamics of migratory birds. In Greenberg, R and PP Marra, eds.  Birds of two worlds. Johns Hopkins University Press, Baltimore.
#'
#' @export



eq06_Bmc <- function(W2,
                     K.bc){

  if( (unlist(W2["mg"])+unlist(W2["mp"])) < K.bc){
    B.mc <- (unlist(W2["mg"])+unlist(W2["mp"]))
  } else{
    B.mc <- K.bc
  }

   #names(B.mc) <- "B.mc"
   return(B.mc)
}


