#' Equation 8: Assigning breeding males to "drain" status
#'
#' @details
#' SUMMER MALE (M) DYNAMICS - drain males
#' Aliased as M.2.drain.eq8()
#' Drain males:: Males that don't find a territory
#' AND don't find a mate (for those in sink)
#' become "drain" males (floaters)
#' mg+mp = total male population
#' minus those that ended up in source habitat (K.bc)
#' minus those that paired w/female in sink habitat (B.fk)
#'
#' Aliased previously as M.2.drain.eq8()
#'
#' @param W2 population vector
#' @param K.bc source carrying capacity
#' @param B.fk females available in sink
#'
#' @return xxx xxxx
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal
#'       interactions in the population dynamics of migratory birds.
#'       In Greenberg, R and PP Marra, eds.  Birds o
#'
#' @export


eq08calcScalar <- function(W2,
                           K.bc,
                           B.fk){
  max(0,
      (W2["mg"]+W2["mp"]-K.bc-B.fk) )
}
