#' Equation 4: Allocate breeding females to Source habitat (B.fc)
#'
#' @description This function outputs B.fc, the number of females
#' high quality "source" habitat.
#'
#' @details
#' This function compares total female poplatuion size to K.bc
#' the souce carrying capacity; if total popualtion of females < K.bc, all females
#' go to the source. If (W2["fg"] + W2["fp"]) > K.bc,
#' then it gets filled up.
#'
#' The output of this function is used in equation 17.
#'
#' Previous alias: F.2.source.eq4.c
#'
#' Part of summer female dynamics
#'
#' @param W2 Population vector produced by W0*W1*W2 (?)
#' @param K.bc carrying capacity of b.reeding grounds sour.c.e habitat
#'
#' @return xxx xxxx
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal
#'       interactions in the population dynamics of migratory birds.
#'       In Greenberg, R and PP Marra, eds.  Birds of two worlds.
#'       Johns Hopkins University Press, Baltimore.
#'
#' @export

eq04 <- function(W2,
                 K.bc
){
  ifelse( (W2["fg"] + W2["fp"])  < K.bc,
          (W2["fg"] + W2["fp"]) ,
          K.bc)
}




