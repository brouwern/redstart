#make dataframe for test functions
#to save use usethis::use_data(test.dat.P,overwrite = T)

test_P_df_maker <- function(wmin=1,wmax=100,wstep=30,
                          kmin = 1, kmax=125,kstep=30){
  ## Set up parameters over which to test
  ## sequence of values for population vectors
  seqW <- seq(wmin,wmax,wstep)

  ##sequence of values for population vectors
  seqK <- seq(kmin,kmax,kstep)

  ## create grid of paramters over which to test
  test.dat.P <- expand.grid(mg = seqW,
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
  for(i in 1:nrow(test.dat.P)){

    #load initial test conditions from test.dat.P
    W2 <- test.dat.P[i,c("mg","mp","fg","fp")]
    params <- test.dat.P[i,c("K.bc","K.bk")]

    #execute allocate functions
    test.dat.P$B.fc[i] <- unlist(eq04_Bfc(W2, params$K.bc))
    test.dat.P$B.fk[i] <- unlist(eq05_Bfk(W2,K.bc =  params$K.bc, K.bk =  params$K.bk))
    test.dat.P$B.mc[i] <- unlist(eq06_Bmc(W2, params$K.bc))
    test.dat.P$B.mk[i] <- unlist(eq07_Bmk(W2,K.bc = params$K.bc, B.fk = test.dat.P$B.fk[i]))
    test.dat.P$B.md[i] <- unlist(eq08_Bmd(W2,K.bc = params$K.bc,B.fk = test.dat.P$B.fk[i]))
  }

return(test.dat.P)
}


