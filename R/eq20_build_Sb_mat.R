#' Equation 20: BREEDING season mortality
#'
#' @details
#' "adult birds experience both sex- and habitat-specific mortality
#' over the breeding season."
#'
#' Previously aliased as Fx.make.breeding.s.matrix.eq20
#'
#' @param S.b.mc survival...
#' @param S.b.mk survival...
#' @param S.b.md survival...
#' @param S.b.fc survival...
#' @param S.b.fk survival...
#'
#' @export


eq20_build_Sb_mat <- function(S.b.mc,
                 S.b.mk,
                 S.b.md,
                 S.b.fc,
                 S.b.fk){
  S.b <- c(S.b.mc, 0.0,    0.0,    0.0,      0.0,
           0.0,    S.b.mk, 0.0,    0.0,      0.0,
           0.0,    0.0,    S.b.md, 0.0,      0.0,
           0.0,    0.0,    0.0,    S.b.fc,   0.0,
           0.0,    0.0,    0.0,    0.0,      S.b.fk)
  S.b <- matrix(S.b, nrow = 5,byrow = T)

  return(S.b)
}

