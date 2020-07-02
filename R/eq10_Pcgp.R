#' Equation 10: proportion males from good winter habitat paired with females from poor winter habitat (P.cgp)
#'
#' The logic of this function is:
#'  1) IF (the number of males from good winter habitat) > (the number of females from good habitat)
#'  AND (the number of females from good) <  (the total K for source habitat)
#'  THEN the number of good-poor pairings is determined by TODO()...
#'  2) ELSE, the number of good-poor pairings is 0 (all pairings are good-good)
#'
#' Note that in the original paper, both equations 10 and 11 have the same subscripts of P.cgp, while in equation 12 it is implied they have different subscripts.  It appears that the 2nd subscript should indicate the winter habitat for males and the 3rd should indicate the winter habitat for females.  Therefore equation 10 in the original paper remains as P.cgp (P.source.male-good.female-poor) and equation 11 should be changed to P.c.pg (P.source.male-poor.female-good)
#'
#' @param W2 population vector
#' @param K.bc Carrying capacity (K) during breeding season (b) of source (c) habitat
#' @param B.mc females available in sink (typo; males?, source?)
#' @param B.fc males on sour.c. habitat (typo, females?)
#' @param ... Additional parameters
#'
#' @return P.cgp proportion males from good winter habitat paired with females from poor winter habitat
#'
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal interactions in the population dynamics of migratory birds. In Greenberg, R and PP Marra, eds.  Birds of two worlds. Johns Hopkins University Press, Baltimore.
#'
#' @examples
#' # Trivial example
#' W2. <- c(10,10,10,10)
#' names(W2.) <- c("mg","fg","mp","fp")
#' eq10_Pcgp(W2 = W2., K.bc = 0, K.bk = 10, B.mc = 5, B.fc =5)
#'
#'
#'
#' @export

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
     P.cgp <- numerator/denominator
    }
  }

  return(P.cgp)
}
