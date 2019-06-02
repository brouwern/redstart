#' Function to test equations 9 through 16
#'
#' These equations determine pairing proportions (P) in breeding habitat.
#'
#' @param test.dat.P dataframe with values against which to test function
#' @param FUN function to test
#' @param plot.test plots a histogram of the proportions calculated by tyeh function
#' @param subsample should the test data be randomly subsampled for quick testing
#' @param sample.N how many subsamples of datarame to take
#' @param return.df return the full dataframe with original test values
#'
#' @import graphics
#'
#' @export

test_P <- function(dat = test.dat.P,
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
  for(i in 1:nrow(test.dat.P)){
    W2 <- test.dat.P[i,c("mg","mp","fg","fp")]
    K.bc <- test.dat.P[i,c("K.bc")]
    K.bk <- test.dat.P[i,c("K.bk")]

    B.fc <- test.dat.P[i,c("B.fc")]
    B.fk <- test.dat.P[i,c("B.fk")]
    B.mc <- test.dat.P[i,c("B.mc")]
    B.mk <- test.dat.P[i,c("B.mk")]
    B.md <- test.dat.P[i,c("B.md")]

    test.dat.P$P[i] <- FUN(W2,
                           K.bc = K.bc, K.bk = K.bk,
                           B.fc = B.fc, B.fk = B.fk,
                           B.mc= B.mc, B.mk = B.mk, B.md = B.md,
                           i = i)

    # if(FUN.name %in% c("eq09_Pcgg", "eq09","9")){
    #   test.dat.P$P[i] <- FUN(W2, K.bc = K.bc, B.mc = B.mc,B.fc = B.fc)
    # }
    #
    # if(FUN.name == "eq10"){
    #   test.dat.P$P[i] <- FUN(W2, K.bc = K.bc, B.fc = B.fc,B.mc = B.mc )
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
    hist(test.dat.P$P, main = paste("N = ",sample.N))

  }

  #print summar
  print(summary(test.dat.P$P))

  #return df
  if(return.df == TRUE){
    return(test.dat.P)
  }

}




