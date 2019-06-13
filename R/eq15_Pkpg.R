#' Equation 15: Proportion in sink poor-good (P.kpg)
#'
#'
#' @details
#'
#'
#' @param W2 Population vector
#' @param K.bc carrying capacity
#' @param K.bk carrying capacity
#' @param B.mk males allocated to sink
#' @param B.fk females allocated to sink
#'
#' @examples
#' # Standard example
#' ## Set population vector and carrying capacities
#' W2. <- c(10, 10,10, 10)
#' W2. <- setNames(W2., c("mg","fg","mp","fp"))
#' K.bc. <- 10
#' K.bk. <- 10
#'
#' ## Assign birds from populatin vector to source or sink habitat using "Bxx" equation
#' B.fc.  <- eq04_Bfc(W2 = W2., K.bc.)
#' B.fk.  <- eq05_Bfk(W2 = W2., K.bc = K.bc., K.bk = K.bk.)
#' B.mc.  <- eq06_Bmc(W2 = W2., K.bc = K.bc.)
#' B.mk.  <- eq07_Bmk(W2 = W2., K.bc = K.bc., B.fk = B.fk.)
#'
#' ## Set up pairings between poor and good using equation 15
#' eq15_Pkpg(W2 = W2., K.bc = K.bc., K.bk = K.bk., B.mk = B.mk., B.fk =B.fk.)
#'
#' # Use the test_P_df_maker() function to generate data
#' ## This isn't very efficient but gets the job done of making test data with less code.  Note that all
#' ## all classes have the same number of birds
#' td <- test_P_df_maker(wmin=10, wmax=10, wstep=1, kmin = 10, kmax=10, kstep=1)
#' W2. <- setNames(td[1:4], c("mg","fg","mp","fp"))
#' eq15_Pkpg(W2 = W2., K.bc = td$K.bc, K.bk = td$K.bk, B.mk = td$B.mk, B.fk = td$B.fk)
#'
#' @return P.kpg Proportion of pairs in sink (k) habitat where male originates from poor (p) and female originated from good (g) winter habitat
#'
#' @export


eq15_Pkpg <- function(W2,
                 K.bc,
                 K.bk,
                 B.mk,
                 B.fk){

  #equation 15 last line
  P.kpg <- 0

  #equation 15 line 1
  if(  W2["mg"] < K.bc){
    if(W2["fg"] >= (K.bc + K.bk)){
      P.kpg <- 1 }
    }


  if(W2["mg"] < K.bc){
    if( K.bc < W2["fg"]){
      if (W2["fg"] < (K.bc+K.bk)){
        numerator <- unlist(W2["fg"])-K.bc
        denominator <- min(B.mk, B.fk)
        P.kpg <- numerator/denominator}
    }
  }


  if(K.bc < W2["mg"]){
    if(W2["mg"] < (K.bc + K.bk)){
      if(W2["fg"] > W2["mg"]){
        numerator <- min(unlist(W2["fg"]), (B.mk+K.bc))-unlist(W2["mg"])
        denominator <- min(B.mk,B.fk)
        P.kpg <-  numerator/denominator}
    }
  }


  P.xxx <- P.kpg
  if(P.xxx > 1){
    warning("Equation produces invalied value")
  }

  if(P.xxx < 0){
    warning("Equation produces invalied value")
  }

  if(is.infinite(P.xxx) == TRUE){
    warning("Equation produces invalied value")
  }

  if(is.na(P.xxx) == TRUE){
    warning("Equation produces invalied value")
  }

  if(is.nan(P.xxx) == TRUE){
    warning("Equation produces invalied value")
  }

  return(P.kpg)
}
