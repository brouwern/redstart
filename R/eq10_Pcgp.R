#' Equation 10:
#'
#' @details
#' proportion of pairs on source habitat
#' composed of MALES   from GOOD and
#'           females from poor
#'  1)If the number of males from good winter habitat is greater than the
#'    the number of females from good habitat
#'    AND the number of females from good is less than the total K for sour.c.e habitat
#'    Then the number of good-poor pairings is determined by...
#'  2)other, the number of good-poor pairings is zero (all pairings are good-good)
#'
#' Note that in the original paper, both equations 10 and 11 have the same subscripts of P.cgp, while in equation 12 it is implied they have different subscripts.  It appears that the 2nd subscript should indicate the winter habitat for males and the 3rd should indicate the winter habitat for females.  Therefore equation 10 in the original paper remains as P.cgp (P.source.male-good.female-poor) and equation 11 should be changed to P.c.pg (P.source.male-poor.female-good)
#'
#' @param W2 population vector
#' @param K.bc females on sour.c.habitat
#' @param B.mc females available in sink
#' @param B.fc males on sour.c. habitat
#' @param ... x
#'
#' @return xxx xxxx
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal
#'       interactions in the population dynamics of migratory birds.
#'       In Greenberg, R and PP Marra, eds.  Birds of 2 worlds
#'
#'
#' @export
#'
#



eq10_Pcgp <- function(W2,
                 K.bc,
                 B.mc,
                 B.fc,
                 ...){

  P.cgp <- 0

  if(W2["mg"] > W2["fg"]){ #if more males from good winter than females from good winter
    if(W2["fg"] < K.bc){   #and females haven't saturated source breeding habitat...
     numerator   <-  (min(unlist(W2["mg"]), unlist(B.fc)) - unlist(W2["fg"]))
     denominator <-  min(unlist(B.mc),     unlist(B.fc))

    #calcualte P.cgp
     #browser()
     P.cgp <- numerator/denominator
    }
  }

  return(P.cgp)
}
