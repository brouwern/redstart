#' QA/QC: Make dataframe for testing functions (internal function)
#'
#' This function generates sets of parameters against which to test the equations 10 to 16, which set up the pairiing frequencies during the breeding seas.  This function has two uses.  First, single sets of parameters can be generated for illustrating that logic of each equation, eg, a population vector (W,) set of carrying capacities (K) and the consequent territory allocations for plugging in to equations 10 to 16.  Second, this function can create an an array of values to plug in to the equations to test that there are no situations where the function generates erroneous output.  Such an array is already stored as test_dat_P
#'
#'
#'
#' Note that the equations sometimes don't work as expected when a K = 0, but this is a special case that isn't likely to be applicable.
#'
#' @param wmin Winter carrying capacity minimum
#' @param wmax Winter carrying capacity max
#' @param wstep step size for creating vector of values between winter min and max
#' @param kmin Summer (Breeding) carrying capacity minimum
#' @param kmax Summer carrying capacity maximum
#' @param kstep step size for creating vector of values between min and max
#'
#' @examples
#'
#' test_P_df_maker(wmin=1,   wmax=10, wstep=5,kmin = 1, kmax=10, kstep=5)
#'
#'
#' # Make a single vector of values
#' ## This is useful for generating data for testing simple conditions;
#' ## this is used in the examples of other function documentation
#' test_P_df_maker(wmin=10,   wmax=10, wstep=1,kmin = 10, kmax=10, kstep=1)
#'
#'
#' @export


test_P_df_maker <- function(wmin=1,   wmax=100, wstep=30,
                            kmin = 1, kmax=125, kstep=30){
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
    W2     <- test_dat_P[i,c("mg","mp","fg","fp")]
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


