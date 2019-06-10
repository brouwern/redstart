#' Equation 2: Build winter survival matrix (S.w)
#'
#' This function produces a diagonal matrix of survival (S.) values.
#' in winter (S.w) for males (.m) and females (.f) in both good (._g)
#' and poor (._p) habitat
#'
#' TODO: I call the matrix S.w; why not W1?
#'
#' @param S.w.mg Survival (S) in the winter (w) of males (m) in good (g) habitat
#' @param S.w.mp Winter survival of males in poor (p) habitat.
#' @param S.w.fg Winter survival of female birds (f) in good habitat
#' @param S.w.fp Winter survival of females in poor habitat
#'
#' @return S.w (W1) matrix of winter (w) survival probabilities by sex and winter habitat
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal
#'       interactions in the population dynamics of migratory birds.
#'       In Greenberg, R and PP Marra, eds.  Birds of two worlds.
#'       Johns Hopkins University Press, Baltimore.
#'
#' @export

eq02buildW1Mat <- function(S.w.mg = 0.80, #survival-winter-male-good habitat
                           S.w.mp = 0.80, #survival-winter-male-poor habitat
                           S.w.fg = 0.80, #survival-winter-female-good habitat
                           S.w.fp = 0.80){#survival-winter-female-poor habitat

  S.w <- c(S.w.mg,    0.00,   0.00,    0.00,
           0.00,      S.w.mp, 0.00,    0.00,
           0.00,      0.00,   S.w.fg,  0.00,
           0.00,      0.00,   0.00,    S.w.fp)

  S.w <- matrix(data = S.w, nrow = 4, byrow = T)

  return(S.w) #CHange this to W1?
 }


