#' Equation 9: Pair formation in source habitat between birds both from
#' good habitat
#'
#' @details
#'
#'   In the SOURCE habitat, how many PAIRS are good-good
#'   (both male and female originate from good wintering habitat)
#' "of all PAIRS in source habitat, the proportion composed of both
#' a male and female FROM good habitat is
#'  This function determines how many females from good winter habitat
#'  pair w/ males from good habitat
#'    1)If W2["mg"] > K.bc  (number of males from good habitat > source K)
#'    AND W2["fg"] > K.bc (number of females from good > source K)
#'    Then all of the pairs on the source habitat will be good-good pairs
#'    That is, if there are enough males and femaels to fill up the source
#'    then the pairing rate will be 100%
#'    This would only occur when there are many birds
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
#'
#' @return xxx, the xxx
#'
#' @export


eq09calcScalar <- function(W2,
                K.bc,
                B.mc,
                B.fc){
  if(  W2["mg"] > K.bc) #If males from good winter habitat exceed the carrying capacity in the sour.c.e  ...
    if(W2["fg"] > K.bc) #AND females from good winter habitat exceed the carring capacity in the sour.ce ...
    {r <-1}             #then all the pairs that form in the source will be good-good

  #ELSE:
  r <- ( min(W2["mg"], W2["fg"]) / #NUMERATOR:   the number from good winter habitat
           min(B.mc, B.fc))          #DENOMINATOR: the total number in the breeding habitat
  r <- ifelse(r > 1, 1,r)
  return(ifelse(r != "NaN",r,0))
}

