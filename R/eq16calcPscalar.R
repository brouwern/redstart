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
#'
#' @export

eq16 <- function(P.kgg,
                 P.kgp,
                 P.kpg,
                 B.mk,
                 B.fk){
  if(B.mk > 0)
    if(B.fk > 0)
    {return(1-P.kgg-P.kgp-P.kpg)}
  return(0)
}
