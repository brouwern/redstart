#' Equation 17: Build vector of Breeding ground abundances after pairing (B0)
#'
#' @param B.mc Abundance of males ("m") in source ("c") habitat on the breeding ground
#' @param B.mk Abundance of males in sink ("k") habitat on the breeding ground
#' @param B.md "Drain" males without territories in source or sink
#' @param B.fc Females in source habitat
#' @param B.fk Females in sink habitat
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal interactions in the population dynamics of migratory birds. In Greenberg, R and PP Marra, eds.  Birds of two worlds. Johns Hopkins University Press, Baltimore.
#'
#' @return B0 vector of breeding ground abundances after pairing of males and females
#'
#' @export

eq17buildB0Vect <- function(B.mc,
         B.mk,
         B.md,
         B.fc,
         B.fk){


#Bundle seperate breeding abundances
B0 <-   c(B.mc,
          B.mk,
          B.md,
          B.fc,
          B.fk)

#name
names(B0) <- c("mc",
               "mk",
               "md",
               "fc",
               "fk")

return(B0)
}


