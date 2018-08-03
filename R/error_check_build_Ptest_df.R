#' Builds sequences of parameters for testing mate pairing equations
#'
#' @examples
#' P.test.df <- error_check_build_Ptest_df()
#' dim(P.test.df)
#'
#' @export
error_check_build_Ptest_df <- function(include.W.0 = FALSE,
                                       include.K.0 = FALSE,
                                       seqW = seq(1,100,30),
                                      seqK =  seq(1,250,50),
                                      seqP.cgg = NA,
                                      seqP.cgp = NA,
                                      seqP.cpg = NA,
                                      seqP.cpp = NA,
                                      seqP.kgg = NA,
                                      seqP.kgp = NA,
                                      seqP.kpg = NA,
                                      seqP.kpp = NA){
  if(include.W.0 == TRUE){
    seqW = seq(0,100,30)
  }

  if(include.K.0 == TRUE){
    seqK = seq(0,100,30)
  }

  P.test.df <- expand.grid(mg = seqW,
                           mp = seqW,
                           fg = seqW,
                           fp = seqW,
                           K.bc = seqK,
                           K.bk = seqK,
                           P.cgg = seqP.cgg,
                           P.cgp = seqP.cgp,
                           P.cpg = seqP.cpg,
                           P.cpp = seqP.cpp,

                           P.kgg = seqP.kgg,
                           P.kgp = seqP.kgp,
                           P.kpg = seqP.kpg,
                           P.kpp = seqP.kpp)

  return(P.test.df)
}
