#' Equation 6: Allocate breeding males to source habitat.
#'
#' @details
#' Summer male (M) dynamces
#' males acquiring  source habitat
#' I believe this allows males to be holding territories
#' w/o pairing w/ a female
#'
#' Aliased as M.2.source.eq6.c()
#'
#' @param W2 Population vector produced by eq02
#' @param K.bc carrying capacity...
#'
#' @return xxx xxxx
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal
#'       interactions in the population dynamics of migratory birds.
#'       In Greenberg, R and PP Marra, eds.  Birds of two worlds.
#'       Johns Hopkins University Press, Baltimore.
#'
#' @export



eq06 <- function(W2,
                 K.bc){

  ifelse( (W2["mg"]+W2["mp"]) < K.bc,
          (W2["mg"]+W2["mp"]),
          K.bc)
}


