#' Equation 3: Make northward migration survival matrix (S.m)
#'
#' This function produces a diagonal matrix of northward (spring)
#' migration (.m) survival rates
#'
#' Previous alias Fx.make.spring.mig.surv.eq3
#'
#' @param S.m.mg Survival (S.) during migration (.m.) of males (.m) originating from good (g) habitat
#' @param S.m.mp Survival on migration of males originating from poor (p) habitat.
#' @param S.m.fg Migration survival of female birds (f) from good habiatat
#' @param S.m.fp Migration survival of females from poor habitat
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal
#'       interactions in the population dynamics of migratory birds.
#'       In Greenberg, R and PP Marra, eds.  Birds of two worlds.
#'       Johns Hopkins University Press, Baltimore.
#'
#' @export


eq03 <- function(S.m.mg = 0.75,
                 S.m.mp = 0.75,
                 S.m.fg = 0.75,
                 S.m.fp = 0.75){

  S.m <- c(S.m.mg, 0.00,   0.00,   0.00,
           0.00,   S.m.mp, 0.00,   0.00,
           0.00,   0.00,   S.m.fg, 0.00,
           0.00,   0.00,   0.00,   S.m.fp)

  S.m <- matrix(data = S.m, nrow = 4, byrow = T)

  return(S.m)
 }


