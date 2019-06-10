#' Equation 25: Competition A.i constraint
#'
#' @param A.i.G.settled.raw.j The raw number of birds that eq24_competition indicates have setteled.  This number needs to be trimmed if necessary so that the number settling cannot exceed the number actively looking for territories
#' @param A.i.active.j The number of birds that were actively looking for nests.
#'
#' NOTE: in previous versions of this A.i.0 was incorrectly used instead
#' of A.i.active.j
#'
#' @return A.i.G.settled.cor.j ....
#'
#' @export

eq25_comp_constrain <- function(A.i.G.settled.raw.j,
                                A.i.active.j){

  #make sure more haven't settled than were available to settle
  A.i.G.settled.cor.j <- ifelse(A.i.G.settled.raw.j >= A.i.active.j,
                                A.i.active.j,
                                A.i.G.settled.raw.j)
  return(A.i.G.settled.cor.j)
}



### original version
# eq25_comp_constrain <- function(A.i.G.settled.raw.j,
#                                 A.i.0){
#
#   A.i.G.settled.cor.j <- ifelse(A.i.G.settled.raw.j > A.i.0,
#                                 A.i.0,
#                                 A.i.G.settled.raw.j)
#   return(A.i.G.settled.cor.j)
# }
