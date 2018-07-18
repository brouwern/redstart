# Equation 25: Competition A.i constraint

eq25_comp_constrain <- function(A.i.settled.j.raw = A.i.settled.j.raw,
                                A.i.0 = A.i.0){
  A.i.settled.j.cor <- ifelse(A.i.settled.j.raw > A.i.0, A.i.0, A.i.settled.j.raw)
  return(A.i.settled.j.cor)
}
