#' Equation 25: Competition A.i constraint
#' @export

eq25_comp_constrain <- function(A.i.G.settled.raw.j,
                                A.i.0){
  A.i.G.settled.cor.j <- ifelse(A.i.G.settled.raw.j > A.i.0,
                                A.i.0,
                                A.i.G.settled.raw.j)
  return(A.i.G.settled.cor.j)
}
