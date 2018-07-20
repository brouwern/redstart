#' Equation 9: Pair formation in source habitat between birds both from
#' good habitat
#'
#' @description Calculats the proportion of pairing in source (.c_) breeding habitat that are betwee
#' males that wintered in good habitat and females that wintered in good habitat.
#'
#' @details
#' This function determines how many females from good winter habitat pair w/ males
#' from good habitat
#' 1)If mg > K.bc  (number of males from good habitat > source K)
#'   AND fg > K.bc (number of females from good > source K)
#'   Then all of the pairs on the source habitat will be good-good pairs
#'   That is, if there are enough males and femaels to fill up the source then
#'   the pairing rate will be 100%
#'   This would only occur when there are many birds
#'    2)Otherwise, the ratio of good-good pairs will be determined
#'      by the ratio of
#'       2a)the number of males or females from good habitat, whichever is less
#'       2b)and the number of males or females on source habitat, whichever is less
#'          (?)
#'
#'  B.fc should usually be lower than B.mc b/c females have higher mort and/or are forced
#'     by competition into poor habitat in the winter, resulting in carry over effects
#'
#' Possible ratios for the 2nd condition
#' W2.mg/B.mc = males from good winter / males settled in source
#' W2.fg/B.mc = females from good winter / males settled in source
#' W2.mg/B.fc = males from good winter / females settled in source
#' W2.fg/B.fc = females from good winter / females settled in source
#' The ratio r is tricky.  It is determined by which sex is limiting in terms of
#' input from good winter habitat (numerator)
#' and which sex is limiting in terms of total abundance in the breeding habitat
#'
#' Aliased as pairing.eq9.P.c.gg() in previous version of coad
#'
#' @param W2 population vector
#' @param K.bc source carrying capacity (?)  original file said "sink popualtion size" in comments
#' @param B.mc females available in sink
#' @param B.fc ...
#' @param ... ...
#'
#' @return P.cgg, the proportion of pairings in the "source" breeding habitat between male and
#' females which both winter in the good habitat
#'
#' @examples
#' # Set parameters:
#' ## Create named population W2 vector using the eq01buildW0vect()
#' W2 <- eq01buildW0vect(100,10,100,10)
#'
#' ##source Breeding habitat  carrying capacity
#' K.bc.110 <- 110
#' K.bc.100 <- 100
#' K.bc.10  <- 10
#'
#' ##Sink (k) Breeding habitat carrying capacity
#' K.bk <- 500
#'
#' ##Number of males and females allocated to source breeding habitat
#' B.mc <- eq06_Bmc(W2, K.bc.100)
#' B.fc <- eq04_Bfc(W2, K.bc.100)
#'
#' eq09_Pcgg(W2, K.bc = 10, B.mc, B.fc)
#'
#' # Test loop
#' seqW <- seq(1,100,30)
#' seqK <- seq(1,125,50)
#' df <- expand.grid(mg = seqW,
#'                   mp = seqW,
#'                   fg = seqW,
#'                   fp = seqW,
#'                   K.bc = seqK,
#'                   K.bk = seqK)
#'
#' df$P.cgg <- NA
#'
#' for(i in 1:nrow(df)){
#'     W2 <- df[i,c("mg","mp","fg","fp")]
#'     params <- df[i,c("K.bc","K.bk")]
#'
#'     B.mc <- eq06_Bmc(W2, params$K.bc)
#'     B.fc <- eq04_Bfc(W2, params$K.bc)
#'     df$P.cgg[i] <- eq09_Pcgg(W2,params$K.bc, B.mc,B.fc)
#' }
#'
#' hist(df$P.cgg)
#' @export


eq09_Pcgg <- function(W2,
                      K.bc,
                      B.fc,
                      B.mc,
                      ...){

  if(  (W2["mg"] > K.bc) &  #If males from good winter habitat exceed souce carrying capacity...
       (W2["fg"] > K.bc)  ){#AND females from g winter habitat exceed souce carrying capacity...
    P.cgg <-1 }           #then all the pairs that form in the source must be good-good
    else {
      P.cgg <-    min(c(unlist(W2["mg"]), unlist(W2["fg"]))) / #NUMERATOR:   the number from good winter habitat
                  min(c(unlist(B.mc),     unlist(B.fc)    ))         #DENOMINATOR: the total number in the breeding habitat

    }


  #check for errors
  if(P.cgg < 0){
    browser()
    message("\nERROR IN EQUATION 9: P < 0!!")
    message("  P = ",P.cgg)
  }


  if(P.cgg > 1){
    #browser()
    message("\nERROR IN EQUATION 9: P > 1!!")
    message("  P = ",P.cgg)
  }

  if(is.nan(P.cgg) == TRUE){
    #browser()
    message("ERROR IN EQUATION 9: NaN!!")
  }


  #paper over errors...
  P.cgg <- ifelse(P.cgg > 1,            1, P.cgg)
  P.cgg <- ifelse(is.nan(P.cgg) == TRUE,0, P.cgg)

  ## Return output
  return(P.cgg)
}

