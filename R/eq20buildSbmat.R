#' Equation 20: Breeding-season mortality matrix (S.b)
#'
#' "Adult birds experience both sex- and habitat-specific mortality over the breeding season" (Runge and Marra 2004, page TODO())
#'
#'
#' @param S.b.mc Survival (S) during summer breeding season (b) of adult males (m) who hold territories on source (c) habitat.
#' @param S.b.mk Survival (S) during summer breeding season (b) of adult males (m) who are nesting in sink (k) habitat.
#' @param S.b.md Survival (S) during summer breeding season (b) of adult males (m) who have "drain" (d) status and do not hold a territory or have a make (aka unpaired males, floaters)
#' @param S.b.fc Survival (S) during summer breeding season (b) of adult females (f) who are nesting source (c) habitat.
#' @param S.b.fk Survival (S) during summer breeding season (b) of adult females (f) who are nesting in on sink (k) habitat.
#'
#' @return S.b Diagonal matrix of breeding-season survival probabilities
#'
#' @examples
#' eq20buildSbmat()
#'
#' @export


eq20buildSbmat <- function(S.b.mc = 0.95,
                 S.b.mk = 0.85,
                 S.b.md = 0.80,
                 S.b.fc = 0.95,
                 S.b.fk = 0.85){
  S.b <- c(S.b.mc, 0.0,    0.0,    0.0,      0.0,
           0.0,    S.b.mk, 0.0,    0.0,      0.0,
           0.0,    0.0,    S.b.md, 0.0,      0.0,
           0.0,    0.0,    0.0,    S.b.fc,   0.0,
           0.0,    0.0,    0.0,    0.0,      S.b.fk)
  S.b <- matrix(S.b, nrow = 5, byrow = T)

  return(S.b)
}

