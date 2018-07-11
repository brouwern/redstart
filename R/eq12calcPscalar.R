#' Equation 12
#'
#' @details
#' SUMMER MALE (M) DYNAMICS - pairing
#'  proportion composed of a male and female both from poor habitat
#'  This is calcualted by subtraction
#'
#' Previously aliased as pairing.eq12.P.cpp()
#'
#' @param P.cgg proportion...
#' @param P.cgp proportion...
#' @param P.cpg proportion...
#'
#' @return P.cpp
#'
#' @export

eq12 <- function(P.cgg,
                 P.cgp,
                 P.cpg){
  1 - P.cgg - P.cgp - P.cpg}
