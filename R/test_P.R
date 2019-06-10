#' QA/AC: Function to test equations 9 through 16
#'
#' These equations determine pairing proportions (P) in breeding habitat.
#'
#' @param dat dataframe with values against which to test function.  Defaults to object named test_dat_P.
#' @param FUN function to test
#' @param plot.test plots a histogram of the proportions calculated by tyeh function
#' @param subsample should the test data be randomly subsampled for quick testing
#' @param sample.N how many subsamples of datarame to take
#' @param return.df return the full dataframe with original test values
#' @param ... Other
#'
#' @import graphics
#'
#' @export

test_P <- function(dat = test_dat_P,
                   FUN = eq09_Pcgg,
                   #FUN.name = "eq09",
                   plot.test = TRUE,
                   subsample = TRUE,
                   sample.N = 10,
                   return.df = FALSE,
                   ...){

  #subsample dataframe to reduce runtime
  if(subsample == TRUE){
    s <- sample(1:nrow(dat), size = sample.N, replace = F)
    dat <- dat[s, ]
  }


  #loop over test df
  for(i in 1:nrow(dat)){
    W2 <- dat[i,c("mg","mp","fg","fp")]
    K.bc <- dat[i,c("K.bc")]
    K.bk <- dat[i,c("K.bk")]

    B.fc <- dat[i,c("B.fc")]
    B.fk <- dat[i,c("B.fk")]
    B.mc <- dat[i,c("B.mc")]
    B.mk <- dat[i,c("B.mk")]
    B.md <- dat[i,c("B.md")]

    dat$P[i] <- FUN(W2,
                           K.bc = K.bc, K.bk = K.bk,
                           B.fc = B.fc, B.fk = B.fk,
                           B.mc= B.mc, B.mk = B.mk, B.md = B.md,
                           i = i)

    # if(FUN.name %in% c("eq09_Pcgg", "eq09","9")){
    #   dat$P[i] <- FUN(W2, K.bc = K.bc, B.mc = B.mc,B.fc = B.fc)
    # }
    #
    # if(FUN.name == "eq10"){
    #   dat$P[i] <- FUN(W2, K.bc = K.bc, B.fc = B.fc,B.mc = B.mc )
    # }

  }

  #if not subsample, set "sample.N" to test dataframe size
  #for labeling
  if(subsample == FALSE){
    sample.N <- nrow(dat)
  }

  #plot distribution of P
  if(plot.test == TRUE){
    par(mfrow = c(1,1))
    hist(dat$P, main = paste("N = ",sample.N))

  }

  #print summar
  print(summary(dat$P))

  #return df
  if(return.df == TRUE){
    return(dat)
  }

}




