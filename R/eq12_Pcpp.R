#' Equation 12
#'
#' @details
#' SUMMER MALE (M) DYNAMICS - pairing
#'  proportion composed of a male and female both from poor habitat
#'  This is calculated by subtraction
#'
#'  Note that in the original paper, equations 10 and 11 both are named "P.cgp" but
#'  in equation 12 it is implied that 10 should be P.cgp while 11 should P.cpg.  THis
#'  would mnake the 2nd subscript for the male condition (P.cg_) and the 3rd subscript
#'  for the female condition (P.c_p)
#'
#' Previously aliased as pairing.eq12.P.cpp()
#'
#' @param P.cgg proportion...
#' @param P.cgp proportion...
#' @param P.cpg proportion...
#' @param ... ....
#'
#' @return P.cpp
#'
#' @export

eq12_Pcpp <- function(P.cgg,
                 P.cgp,
                 P.cpg,...){
  P.cpp <-  round(1 - P.cgg - P.cgp - P.cpg,5)

  #warnings
  if(P.cpp > 1){
    #browser()
    message("Error in equation 12: P.cpp > 1;")
    message(paste(round(P.cgg,3) , P.cgp , P.cpg, sep = ","))

    #fix
    P.cpp <- 1
  }

  if(P.cpp < 0){
    #browser()
    message("Error in equation 12: P.cpp < 0;")
    message(paste(round(P.cgg,3) , P.cgp , P.cpg, sep = ","))
  }

  return(P.cpp)
  }
