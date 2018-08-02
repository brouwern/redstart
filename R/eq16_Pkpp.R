#' Equation 16:
#'
#' @details
#' Proportion in sink composed of poor-poor
#'
#' Previously aliased as pairing.eq16.P.kpp
#'
#' @param P.kgg ...
#' @param P.kgp ...
#' @param P.kpg ...
#' @param B.mk ...
#' @param B.fk ...
#' @param ... ...
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

  #if(P.kpp == 1){browser()}
  return(P.kpp)
}
