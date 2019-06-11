#' Equation 22: Mortality during migration of young/offspring (S.y)
#'
#' @details
#' Previously aliased as Fx.make.fall.adult.s.matrix.eq22
#'
#' @param S.y.mc survival...
#' @param S.y.mk survival...
#' @param S.y.fc survival...
#' @param S.y.fk survival...
#'
#' @example
#' eq22buildSymat()
#'
#' @export



eq22buildSymat <- function(S.y.mc = 0.80,
                 S.y.mk = 0.75,
                 S.y.fc = 0.80,
                 S.y.fk = 0.75){
  S.y <- c(S.y.mc, 0.0,    0.0,      0.0,
           0.0,    S.y.mk, 0.0,      0.0,
           0.0,    0.0,    S.y.fc,   0.0,
           0.0,    0.0,    0.0,      S.y.fk)
  S.y <- matrix(S.y, nrow = 4,byrow = T)
  return(S.y)
}
