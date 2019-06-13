#' Equation 9: Pair formation in source habitat between birds which both originated from good winter habitat (P.cgg)
#'
#' Calculates the proportion of pairs in source (.c) breeding habitat that occur between males that wintered in good habitat and females that wintered in good habitat.
#'
#' This function determines how many females from good winter habitat pair w/ males
#' from good habitat.  The logic of this function is:
#' 1)If mg > K.bc  (number of males from good habitat > source K) AND fg > K.bc (number of females from good > source K) THEN all of the pairs on the source habitat will be good-good pairs.  That is, if there are enough males and females to fill up the source then the good-good the pairing rate will be 100%. This would only occur when there are not many birds
#' 2)ELSE, the ratio of good-good pairs will be determined by the ratio of
#'    2a) The number of males OR females from good habitat, whichever is less
#'    2b) AND the number of males or females on source habitat, whichever is less
#'
#'
#'  B.fc should usually be lower than B.mc because females have lower survival and/or are forced by competition into poor habitat in the winter, resulting in carry over effects.
#'
#' Possible ratios for the 2nd condition
#' W2.mg/B.mc = males from good winter / males settled in source
#' W2.fg/B.mc = females from good winter / males settled in source
#' W2.mg/B.fc = males from good winter / females settled in source
#' W2.fg/B.fc = females from good winter / females settled in source
#'
#' The ratio r is tricky.  It is determined by which sex is limiting in terms of input from good winter habitat (numerator) AND which sex is limiting in terms of total abundance in the breeding habitat.
#'
#' @param W2 population vector
#' @param K.bc source (c) carrying capacity (K) in breeding season (b)original file said "sink popualtion size" in comments
#' @param B.mc Number of males (.m) settled into source (.xc) habitat
#' @param B.fc females available in sink
#' @param ... Aditional parameters
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal interactions in the population dynamics of migratory birds. In Greenberg, R and PP Marra, eds.  Birds of two worlds. Johns Hopkins University Press, Baltimore.
#'
#' @return P.cgg, the proportion of pairings in the "source" breeding habitat between male and females which both wintered in the good habitat
#'
#' @examples
#'
#' # Test eq09_Pcgg() for single sets of parameters
#' ## Set parameters:
#' ### Population size:
#' ### Create named population W2 vector using eq01buildW0vect()
#' ### Note: ratio of good male to good females is 10:1
#' W2 <- eq01buildW0vect(10,10,
#'                       1, 10)
#' ### source Breeding habitat carrying capacities (K.bc)
#' #### 3 options
#' K.bc.10  <- 10
#' K.bc.1   <- 1
#' K.bc.0   <- 0
#'
#' ### Sink (k) Breeding habitat carrying capacity (K.bk)
#' K.bk <- 500
#'
#' ### Number of males and females allocated to source breeding habitat
#' #### use eq04_Bfc() & eq06_Bmc()
#'
#' ##### K.bc = 10
#' B.fc.K10 <- eq04_Bfc(W2, K.bc.10)
#' B.mc.K10 <- eq06_Bmc(W2, K.bc.10)
#'
#' ##### K.bc = 1
#' B.fc.K1  <- eq04_Bfc(W2, K.bc.1)
#' B.mc.K1  <- eq06_Bmc(W2, K.bc.1)
#'
#' ##### K.bc = 0
#' B.fc.K0  <- eq04_Bfc(W2, K.bc.0)
#' B.mc.K0  <- eq06_Bmc(W2, K.bc.0)
#'#'
#' #### K.bc = 10
#' ##### 10 source territories, 10 good males, but only 1 good female
#' eq09_Pcgg(W2, K.bc = K.bc.10, B.fc = B.fc.K10,  B.mc = B.mc.K10)
#'
#' #### K.bc = 1
#' ##### 1 source territory, 10 good males, but only 1 good female
#' eq09_Pcgg(W2, K.bc = K.bc.1,  B.fc = B.fc.K1,   B.mc = B.mc.K1)
#'
#' #### K.bc = 0
#' ##### 0 source territories, 10 good males, but only 1 good female
#' eq09_Pcgg(W2, K.bc = K.bc.0,  B.fc = B.fc.K0,   B.mc = B.mc.K0)
#'
#' # Test eq09_Pcgg() accross a range of parameter
#' dim(test_dat_P)
#' P.test.df.out <- test_P(test_dat_P, FUN = eq09_Pcgg, return.df = TRUE, plot.test = FALSE)
#'
#' ## Look at distribution of P.cgg
#' hist(P.test.df.out$P)
#' summary(P.test.df.out$P)
#' @export


eq09_Pcgg <- function(W2,
                      K.bc,
                      B.mc,
                      B.fc,
                      ...){

  #equation 9: NEW CONDITION
  #  in original R-M set up, if K.bc happend to be 0, then 1 was returned
  #  K.bc is unlikely to be set to 0, and even if it was, its not likely
  #  to have any consequences, but this was changed just in case

  if(K.bc == 0){
    P.cgg <-  0
    return(P.cgg)
  }

  #equation 9: NEW CONDITION
  # unlikely to happen but could be useful if code adapated for PVA
  if(W2["mg"] == 0 & W2["fg"] == 0){
    P.cgg <-  0
    return(P.cgg)
  }

  #equation 9: NEW CONDITION
  # unlikely to happen but could be useful if code adapated for PVA
  if(W2["mg"] > 0 & W2["fg"] == 0 |
     W2["mg"] == 0 & W2["fg"] > 0){
    P.cgg <-  0
    return(P.cgg)
  }


  #equation 9 part A
  if((W2["mg"] > K.bc) &#If males fr. good win hab exceed souce K...
    (W2["fg"] > K.bc) & #AND fem. fr. g winter hab exceed souce K...
       K.bc      > 0){  #(NEW:) AND K.bc doesn't=0 for some strange reason...
    P.cgg <-1           #THEN all the pairs formed in the source must be g-g
    return(P.cgg)
  }

  #equation 9 part B
  if(K.bc > 0){
    numerator   <- min(c(unlist(W2["mg"]),
                         unlist(W2["fg"])))#num from good winter hab
    denominator <- min(c(unlist(B.mc),
                         unlist(B.fc)    ))#tot num in the breeding hab
    P.cgg <-  numerator/denominator
    return(P.cgg)
  }



}

