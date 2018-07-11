#' Equation 10:
#'
#' @details
#' SUMMER MALE (M) DYNAMICS - pairing dynamics
#' proportion of pairs on source habitat
#' composed of MALES   from GOOD and
#'           females from poor
#'  1)If the number of males from good winter habitat is greater than the
#'    the number of females from good habitat
#'    AND the number of females from good is less than the total K for sour.c.e habitat
#'    Then the number of good-poor pairings is determined by...
#'  2)other, the number of good-poor pairings is zero (all pairings are good-good)
#'
#' Aliased prevously as pairing.eq10.P.c.gp
#'
#' @param W2 population vector
#' @param K.bc females on sour.c.habitat
#' @param B.fc females available in sink
#' @param B.mc males on sour.c. habitat
#'
#' @return xxx xxxx
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal
#'       interactions in the population dynamics of migratory birds.
#'       In Greenberg, R and PP Marra, eds.  Birds o
#'
#' @export

eq10 <- function(W2,
                 K.bc,
                 B.fc,
                 B.mc){
  if(W2["mg"] > W2["fg"])
    if(W2["fg"] < K.bc)
    {num <- (min(W2["mg"], B.fc) - W2["fg"])  #this code checks for division by zero
    denom <- min(B.mc, B.fc)
    return( ifelse(num/denom != "NaN", num/denom,0))}

  return(0)
}
