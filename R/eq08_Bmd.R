#' Equation 8: Assigning breeding males to "drain" (aka "floater") status
#'
#' SUMMER MALE (M) DYNAMICS - drain males
#' Drain males: Males that don't find a territory AND don't find a mate (for those in sink)
#' become "drain" males (floaters)
#' mg+mp = total male population
#' minus those that ended up in source habitat (K.bc)
#' minus those that paired w/female in sink habitat (B.fk)
#'
#' Note that it is possible for the equation W2.mg+W2.mp - K.bc to return a negative value if the number
#' of males is less than the source habitat size K.bc.  This is why  max() is used
#' to corred for this.
#'
#'
#' @param W2 population vector
#' @param K.bc source carrying capacity
#' @param B.fk females available in sink
#'
#' @return Number of males in drain habitat.
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal
#'       interactions in the population dynamics of migratory birds.
#'       In Greenberg, R and PP Marra, eds.  Birds o
#'
#' @export


eq08_Bmd <- function(W2,
                           K.bc,
                           B.fk){
  max(0,
      (unlist(W2["mg"])+unlist(W2["mp"]) - K.bc - unlist(B.fk)) )
}
