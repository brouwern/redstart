#' make dataframe for testing functions
#' to save use usethis::use_data(test_dat_P,overwrite = T)
#'
#' @param wmin Winter carrying capacity minimum
#' @param wmax Winter carrying capacity max
#' @param wstep step size for creating vector of values between winter min and max
#' @param kmin Summer (Breeding) carrying capacity minimum
#' @param kmax Summer carrying capacity maximum
#' @param kstep step size for creating vector of values between min ans max
#'
#' @export


test_P_df_maker <- function(wmin=1, wmax=100,wstep=30,
                            kmin = 1, kmax=125,kstep=30){
  ## Set up parameters over which to test
  ## sequence of values for population vectors
  seqW <- seq(wmin,wmax,wstep)

  ##sequence of values for population vectors
  seqK <- seq(kmin,kmax,kstep)

  ## create grid of paramters over which to test
  test_dat_P <- expand.grid(mg = seqW,
                            mp = seqW,
                            fg = seqW,
                            fp = seqW,
                            K.bc = seqK,
                            K.bk = seqK,
                            B.fc = NA,
                            B.fk = NA,
                            B.mc = NA,
                            B.mk = NA,
                            B.md = NA,
                            P = NA)

  ## Loop over equation being tested
  for(i in 1:nrow(test_dat_P)){

    #load initial test conditions from test_dat_P
    W2 <- test_dat_P[i,c("mg","mp","fg","fp")]
    params <- test_dat_P[i,c("K.bc","K.bk")]

    #execute allocate functions
    test_dat_P$B.fc[i] <- unlist(eq04_Bfc(W2, params$K.bc))
    test_dat_P$B.fk[i] <- unlist(eq05_Bfk(W2,K.bc =  params$K.bc, K.bk =  params$K.bk))
    test_dat_P$B.mc[i] <- unlist(eq06_Bmc(W2, params$K.bc))
    test_dat_P$B.mk[i] <- unlist(eq07_Bmk(W2,K.bc = params$K.bc, B.fk = test_dat_P$B.fk[i]))
    test_dat_P$B.md[i] <- unlist(eq08_Bmd(W2,K.bc = params$K.bc,B.fk = test_dat_P$B.fk[i]))
  }

return(test_dat_P)
}


