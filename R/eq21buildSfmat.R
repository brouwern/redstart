#' Equation 21 - MIGRATION Mortality of ADULTS
#' "Mort during mig depends upon the sex of the bird and breeding habitat it used
#'
#' Aliased previously Fx.make.fall.adult.s.matrix.eq21
#'
#' @param S.f.mc survival...
#' @param S.f.mk survival...
#' @param S.f.md survival...
#' @param S.f.fc survival...
#' @param S.f.fk survival...
#'
#' @param S.f Matrix of fall survival probabilities
#'
#' @export


eq21buildSfmat <- function(S.f.mc,
                 S.f.mk,
                 S.f.md,
                 S.f.fc,
                 S.f.fk){
  S.f <- c(S.f.mc, 0.0,    0.0,    0.0,      0.0,
           0.0,    S.f.mk, 0.0,    0.0,      0.0,
           0.0,    0.0,    S.f.md, 0.0,      0.0,
           0.0,    0.0,    0.0,    S.f.fc,   0.0,
           0.0,    0.0,    0.0,    0.0,      S.f.fk)
  S.f <- matrix(S.f, nrow = 5,byrow = T)

  return(S.f)
}
