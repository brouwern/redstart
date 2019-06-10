#' Equation 20: Breeding season mortality matrix (S.b)
#'
#' @details
#' "adult birds experience both sex- and habitat-specific mortality
#' over the breeding season."
#'
#'
#' @param S.b.mc survival...
#' @param S.b.mk survival...
#' @param S.b.md survival...
#' @param S.b.fc survival...
#' @param S.b.fk survival...
#'
#' @return S.b Matrix of breed-ding season survival probabilities
#'
#' @export


eq20buildSbmat <- function(S.b.mc,
                 S.b.mk,
                 S.b.md,
                 S.b.fc,
                 S.b.fk){
  S.b <- c(S.b.mc, 0.0,    0.0,    0.0,      0.0,
           0.0,    S.b.mk, 0.0,    0.0,      0.0,
           0.0,    0.0,    S.b.md, 0.0,      0.0,
           0.0,    0.0,    0.0,    S.b.fc,   0.0,
           0.0,    0.0,    0.0,    0.0,      S.b.fk)
  S.b <- matrix(S.b, nrow = 5, byrow = T)

  return(S.b)
}

