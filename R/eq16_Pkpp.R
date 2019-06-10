#' Equation 16: Proportion in sink composed of poor-poor pairings (P.kpp)
#'
#'
#' @param P.kgg Proportion source (k) composed of good-good pairings.
#' @param P.kgp Proportion source (k) composed of good-poor pairings.
#' @param P.kpg Proportionin source (k) composed of good-good pairings.
#' @param B.mk males (m) allocated to sink (k)
#' @param B.fk females allocated to sink
#' @param ... xxx
#'
#' @return P.kpp Proportion of pairs in sink (k) which are poor-poor pairings
#'
#' @export

eq16_Pkpp <- function(P.kgg,
                 P.kgp,
                 P.kpg,
                 B.mk,
                 B.fk, ...){

  #equation 16 part 2 (lower line)
  P.kpp <- 0

  #equation 16 part 1 (upper line)
  if(B.mk > 0){
    if(B.fk > 0){
      P.kpp <- 1-P.kgg-P.kgp-P.kpg
      }
    }

  #error check
  if(P.kpp > 1 | P.kpp < 0){
    browser()
    }

  return(P.kpp)
}
