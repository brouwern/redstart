#' Equation 12: Proportion of prairs between a male and female both from poor habitat (P.cpp)
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
#' @param P.cgg Proportion of pairs in source (c) where male and female both originated from good (gg) winter habitat
#' @param P.cgp Proportion of pairs in source (c) where male originated from good (g) and female originated from poor winter habitat
#' @param P.cpg Proportion of pairs in source (c) where male originated from poor (p) and female originated from good (g) winter habitat
#' @param ... Additional parameters
#'
#' @return P.cpp Proportion of prairs between a male and female both from poor habitat
#'
#' @export

eq12_Pcpp <- function(P.cgg,
                 P.cgp,
                 P.cpg,...){
  P.cpp <-  round(1 - P.cgg - P.cgp - P.cpg, 5)

  if(P.cpp > 1| P.cpp < 0){
    browser()
  }

  return(P.cpp)
  }
