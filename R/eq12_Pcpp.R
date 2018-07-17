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

eq12_Pcpp <- function(P.cgg,
                 P.cgp,
                 P.cpg){
  P.cpp <-  round(1 - P.cgg - P.cgp - P.cpg,5)

  #warnings
  if(P.cpp > 1){
    browser()
    message("P.cpp > 1")
  }

  if(P.cpp < 0){
    browser()
    message("P.cpp < 0")
  }

  return(P.cpp)
  }
