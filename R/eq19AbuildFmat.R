#' Equation 19: Matrix of ... (f.mat)
#'
#' sex ratio matrix (equation 19)
#'
#' @param f Fecundity?
#'
#' @return f.mat matrix of xxx
#'
#' @export
#'
#' @examples
#' # Sex ratio matrix
#' eq19AbuildFmat()


eq19AbuildFmat <- function(f = 0.5){
  f.mat <- c(1-f, 0,
             0,   1-f,
             f,   0,
             0,   f)

  f.mat <- matrix(f.mat, ncol = 2, byrow=T)

  return(f.mat)
}

