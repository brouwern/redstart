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
#'       In Greenberg, R and PP Marra, eds.  Birds of 2 worlds
#'
#' @examples
#' # Test loop
#' ## Values for population vector W2
#' seqW <- seq(1,100,30)
#'
#' ## Values for carrying capacities
#' seqK <- seq(1,125,50)
#'
#' ## dataframe of parameter combinations
#' df <- expand.grid(mg = seqW,
#'                   mp = seqW,
#'                   fg = seqW,
#'                   fp = seqW,
#'                   K.bc = seqK,
#'                   K.bk = seqK,
#'                   P.cgg = NA)
#'
#' ## Loop over all param combos
#' for(i in 1:nrow(df)){
#'     W2 <- df[i,c("mg","mp","fg","fp")]
#'     params <- df[i,c("K.bc","K.bk")]
#'
#'     B.mc <- eq06_Bmc(W2, params$K.bc)
#'     B.fc <- eq04_Bfc(W2, params$K.bc)
#'     df$P.cgg[i] <- eq10_Pcgp(W2 = W2,
#'                              K.bc = params$K.bc,
#'                              B.fc = B.fc,
#'                              B.mc = B.mc)
#' }
#'
#' hist(df$P.cgg)
#'
#' @export

eq10_Pcgp <- function(W2,
                 K.bc,
                 B.fc,
                 B.mc){

  P.cgp <- 0

  if(W2["mg"] > W2["fg"]){ #if more males from good winter than females from good winter
    if(W2["fg"] < K.bc){   #and females haven't saturated source breeding habitat...
     num   <-  (min(unlist(W2["mg"]), unlist(B.fc)) - unlist(W2["fg"]))   #this code checks for division by zero
     denom <-  min(unlist(B.mc),     unlist(B.fc))

    #calcualte P.cgp
     #browser()
     P.cgp <- num/denom
    }
  }

  #warning message

  if(P.cgp > 1){
    message("ERROR IN EQUATION 10: P.cgp > 1")
  }

  if(is.nan(unlist(P.cgp)) == TRUE){
      message("ERROR IN EQUATION 10: NaN!!")
    }


  return(P.cgp)
}