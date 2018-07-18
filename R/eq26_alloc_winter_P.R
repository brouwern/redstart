#equation 26 Allocate to poor habitat

eq26_alloc_winter_P <- function(A.i,A.G.i){
  A.P.i <- A.i-A.G.i
  return(A.P.i)
}


