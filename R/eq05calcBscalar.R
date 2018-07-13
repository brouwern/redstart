#' Equation 5: Allocate breeding females to sink habitat (B.fk)
#'
#' @description This function allocates breeding (B.) females (f) to
#' to sink habitat (k).
#'
#'
#' @details
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
#' @param K.bc sour.c.e K
#' @param K.bk sin.k. K
#' @param W2 population vector
#'
#' @return K.bk, the ... xxxx
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal
#'       interactions in the population dynamics of migratory birds.
#'       In Greenberg, R and PP Marra, eds.  Birds of two worlds.
#'       Johns Hopkins University Press, Baltimore.
#'
#'
#' @export


eq05calcScalar <- function(K.bc,
                           K.bk,
                           W2){
  if( (W2["fg"] + W2["fp"]) < K.bc)
  { return(0) }

  if( K.bc <= (W2["fg"] + W2["fp"]) )
    if( (W2["fg"] + W2["fp"]) < (K.bc+K.bk) )
    {return( W2["fg"] + W2["fp"] - K.bc) }

  if( (W2["fg"] + W2["fp"]) >= (K.bc+K.bk))
  { return(K.bk) }
}


