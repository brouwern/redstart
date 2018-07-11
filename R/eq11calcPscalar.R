#' Equation 11:
#'
#' @details
#' SUMMER MALE (M) DYNAMICS
#' Aliased as pairing.eq11.P.cpg()
#'   proportion of poor males mated w/ "good" female
#'   1)If males that wintered in good habitat > females that wintered in good habitat
#'     BUT males that wintered in good less than source K
#'     Then the proportion of poor-good pairings is
#'     a function of
#'   Otherwise, there are no poor-good pairings
#'
#' Previously aliased as pairing.eq11.P.cpg
#'
#' @param W2 population vector; indicats where birds are coming from
#' @param K.bc females  sour.c.habitat carrying capacity
#' @param B.fc females alreadiy in source
#' @param B.mc males already in source
#'
#' @return xxx xxxx
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal
#'       interactions in the population dynamics of migratory birds.
#'       In Greenberg, R and PP Marra, eds.  Birds o
#'
#' @export


eq11 <- function(W2,
                                       B.mc, #
                                       B.fc, #
                                       K.bc){# K
  if(W2["mg"] < W2["fg"])  #original: W2["mg"] > W2["fg"]
    if(W2["mg"] < K.bc)
    {num <-  (min( W2["fg"], B.mc) - W2["mg"])
    denom <- min( B.mc,     B.fc)

    r <-  ifelse(num/denom != "NaN", num/denom,0)

  return( ifelse(r < 0,0,r) )}

  return(0)
}
