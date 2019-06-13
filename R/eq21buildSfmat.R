#' Equation 21: Migration mortality matrix for adults (S.f)
#'
#' "Mort during migration depends upon the sex of the bird and breeding habitat it used" (Runge and Marra 2004, page xxx)
#'
#' @param S.f.mc Survival (S) during fall migration (f) of males (m) originating from source (c) habitat.
#' @param S.f.mk Survival (S) during fall migration (f) of males (m) originating from sink (k) habitat.
#' @param S.f.md Survival (S) during vall migration (f) of males (m) who had "drain" (d) status, aka floaters or unpaired males.
#' @param S.f.fc Survival (S) during fall migration (f) of females (f) originating from source (c) habitat.
#' @param S.f.fk Survival (S) during fall migration (f) of females (f) originating from sink (k) habitat.
#'
#' @param S.f Diagnoal matrix of fall survival probabilities for adults
#'
#' @example
#' eq21buildSfmat()
#'
#' @export


eq21buildSfmat <- function(S.f.mc = 0.80,
                 S.f.mk = 0.75,
                 S.f.md = 0.80,
                 S.f.fc = 0.80,
                 S.f.fk = 0.75){
  S.f <- c(S.f.mc, 0.0,    0.0,    0.0,      0.0,
           0.0,    S.f.mk, 0.0,    0.0,      0.0,
           0.0,    0.0,    S.f.md, 0.0,      0.0,
           0.0,    0.0,    0.0,    S.f.fc,   0.0,
           0.0,    0.0,    0.0,    0.0,      S.f.fk)
  S.f <- matrix(S.f, nrow = 5,byrow = T)

  return(S.f)
}
