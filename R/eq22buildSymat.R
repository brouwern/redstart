#' Equation 22: Mortality during migration of young/offspring (S.y)
#'
#'
#' @param S.y.mc Survival of young (y; aka juveniles) males (m) originating from source (c) habitat
#' @param S.y.mk Survival of young (y) males (m) originating from sink (k) habitat
#' @param S.y.fc Survival of young (y) females (f) originating from source (c) habitat
#' @param S.y.fk Survival of young (y) females (f) originating from sink (k) habitat
#'
#' @return S.y Matrix of survival probabilities for young.
#'
#' @examples
#'
#' # Return default values
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
