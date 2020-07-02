#' Equation 8: Assigning breeding males to "drain" (aka "floater") status
#'
#' Males that don't find a territory AND don't find a mate become "drain" males (floaters).
#' \code{W2.mg+W2.mp} = total male population at the end of spring migration.  From thid gets subtracted birds that ended up in source habitat (\code{K.bc}) and  those that paired with female in sink habitat (\code{B.fk})
#'
#' Note that it is possible for the equation \code{W2.mg+W2.mp - K.b} to return a negative value if the number of males is less than the source habitat size (\code{K.bc}).  The use of \code{max()} corrects for this.
#'
#'
#' @param W2 population vector
#' @param K.bc source carrying capacity
#' @param B.fk females available in sink
#'
#' @return Number of males in drain habitat.
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal interactions in the population dynamics of migratory birds. In Greenberg, R and PP Marra, eds.  Birds of Two Worlds.
#'
#' @examples
#' W2. <- c(10,10,10,10)
#' names(W2.) <- c("mg","fg","mp","fp")
#' eq08_Bmd(W2 = W2., K.bc = 0, B.fk =5)
#'
#'
#' @export


eq08_Bmd <- function(W2,
                     K.bc,
                     B.fk){
  max(0,
      (unlist(W2["mg"])+unlist(W2["mp"]) - K.bc - unlist(B.fk)) )
}
