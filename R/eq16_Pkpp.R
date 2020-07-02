#' Equation 16: Proportion of pairs in sink (k) composed of poor-poor pairings (P.kpp)
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
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal interactions in the population dynamics of migratory birds. In Greenberg, R and PP Marra, eds.  Birds of two worlds. Johns Hopkins University Press, Baltimore.
#'
#' @examples
#' # Trivial example
#' W2. <- c(10,10,10,10)
#' names(W2.) <- c("mg","fg","mp","fp")
#' eq16_Pkpp(P.kgg = 1, P.kgp = 0, P.kpg = 0, B.mk = 5, B.fk =5)
#'
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
    warning("Equation produces invalied value")
    }

  return(P.kpp)
}
