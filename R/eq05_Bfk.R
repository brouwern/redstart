#' Equation 5: Allocate breeding females to sink habitat (B.fk)
#'
#' This function allocates breeding (B.) females (f) to
#' to sink habitat (k).
#'
#' This function compares total number of females fg + fp
#' against K.bc, the  carrying capacity ("K.") of the breeding-ground
#' (".b.") source habiat (".c")
#'
#' Notes:
#' 1)If fg + fp < K.bc, then ALL birds went to the sour.c.e
#' (via the previous function) and 0 birds end up going to the breeding sink
#'
#' 2)If K.bc < fg + fp, then the total female pop is larger
#' the source carrying capacity and all birds cannot go to source.
#' (That is, the nubmer of females is in excess of the amount of source habitat)
#' Therefore, some birds will go to source and some to sink,
#' with birds wintering in high quality habitat having preference
#'
#' So, if K.bc < (fg + fp) AND
#' fg + fp < (K.bc+K.bk)
#' (tot fem pop less than combined source and sink k)
#' then number that go to the sink is fg + fp - K.bc
#' That is, the number going to the sink is anything left after
#' allocation to the source K.bc
#'
#' 3)If fg + fp >= (K.bc+K.bk)
#' (tot fem pop >= combined K of both habitats)
#' then both the source and the sink get completely filled up
#' and any remaining female birds, from the perspective of the model
#' die (!)
#'
#' This is explained by the authors: "dd is implicit in EQUATION (5):
#' females displaced from sink habitat are presumed to die.  This
#' is a 'ceiling' form of dd - no effect is evident until the number of
#' arriving females exceeds the combined carrying capacity of the
#' source and sink habitats" (Runge and Marra 2004, pg 378)
#'
#' Aliased as F.2.sink.eq5.k()
#'
#' @param W2 Population vector produced by eq02buildW1Mat()
#' @param K.bc Carrying capacity (K) during breeding season (b) of source (c) habitat
#' @param K.bk Carrying capacity (K) during breeding season (b) of sink (k) habitat
#'
#' @return B.fk, the total number of females allocated to sink
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
#' #Breeding source habitat carrying capacity
#' K.bc.110 <- 110
#' K.bc.100 <- 100
#' K.bc.10  <- 10
#'
#' #Breeding sink habitat carrying capacity
#' K.bk <- 1000
#'
#' # Total population is 110 and source habitat size is 110, so 0 go to sink
#' eq05_Bfk(W2 = W2, K.bc = K.bc.110, K.bk = K.bk)
#'
#' # Reduce source habitat size down to 100 so 10 go to sink
#' eq05_Bfk(W2 = W2, K.bc = K.bc.100, K.bk = K.bk)
#'
#' # Redeuce source habitat size down to 10, so 100 go to sink
#' eq05_Bfk(W2 = W2, K.bc = K.bc.10, K.bk = K.bk)
#'
#' @export


eq05_Bfk <- function(W2,
                     K.bc,
                     K.bk){
  #if total female pop < carrying capacity in source
  #0 females settle in sink, b/c they have
  #already all settle in source, so retunr 0
  if( (W2["fg"] + W2["fp"]) < K.bc){
    B.fk <- 0 }

  #if total female population is greater than
  #source size BUT is less than total breeding habitat size (K.bc+K.bk)
  #then the number allocated to sink is the total population
  #minus those already allocated to the source
  if( K.bc <= (W2["fg"] + W2["fp"]) ){
    if( (W2["fg"] + W2["fp"]) < (K.bc+K.bk) ){
      B.fk <- (unlist(W2["fg"]) + unlist(W2["fp"])) - K.bc
      }
  }

  #if total female population exceeds carrying capacity of
  #BOTH habitats, then sink will become saturated
  #so return sink size K.bk
  if( (unlist(W2["fg"]) + unlist(W2["fp"])) >= (K.bc+K.bk)){
    B.fk <- K.bk
    }

  #names(B.fk) <- "B.fk"

  return(B.fk)
}


