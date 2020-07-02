#' Equation 11: Proportion of males in source (c) from poor winter habitat (p) mated with females from good (g) winter habitat (P.cpg)
#'
#' The logic of this function is:
#' IF (abundance of males that wintered in good habitat) > (abundance of females that wintered in good habitat)
#' BUT (abundance of males that wintered in good) < than source K,
#' THEN the proportion of poor-good pairings is a function of xxx.
#' ELSE, there are no poor-good pairings
#'
#' Note that in the original paper, both equations 10 and 11 have the same subscripts of P.cgp, while in equation 12 it is implied they have different subscripts.
#' It appears that the 2nd subscript should indicate the winter habitat for males and the 3rd should indicate the winter habitat for females.
#' Therefore equation 10 in the original paper remains as P.cgp (P.source.male-good.female-poor) and equation 11 should be changed to P.c.pg (P.source.male-poor.female-good)
#'
#' Note  W2.mg > W2.fg in original paper.  "\code{>}" needs to be "code{<}"  because poor males end up
#' with good female when there are not enough good males for all the good females (Wmg < Wfg)
#'
#' @param W2 population vector; indicates where birds are coming from
#' @param K.bc Carrying capacity during breeding season in source habitat
#' @param B.mc males already in source
#' @param B.fc females alreadiy in source
#' @param ... Additional parameters
#'
#' @return P.cpg (vector):  the proportion of pairings in the source habitat (c) made up of males from poor winter habitat (p) and females from good winter habitat (g).  See note above about how subscripts are wrong in original paper.
#'
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal interactions in the population dynamics of migratory birds. In Greenberg, R and PP Marra, eds.  Birds of two worlds. Johns Hopkins University Press, Baltimore.
#'
#' @examples
#' # Trivial example
#' W2. <- c(10,10,10,10)
#' names(W2.) <- c("mg","fg","mp","fp")
#' eq11_Pcpg(W2 = W2., K.bc = 0, K.bk = 10, B.mc = 5, B.fc =5)
#'
#'
#' @export


eq11_Pcpg <- function(W2,
                      K.bc,
                      B.mc,
                      B.fc,
                      ...){

  #part B of equation 11 (lower part)
  P.cpg <- 0

  #part A of equation 11 (lower part)
  if(W2["mg"] < W2["fg"]){  # W2["mg"] > W2["fg"] in paper.  ">" needs to be "<"  because poor males end up with good female when there are not enough good males for all the good females (Wmg < Wfg)

    if(W2["mg"] < K.bc){
      num <-  (min( unlist(W2["fg"]), unlist(B.mc)) - unlist(W2["mg"]))
      denom <- min( unlist(B.mc),     unlist(B.fc))

      P.cpg <- num/denom

    }

  }

  return(P.cpg)
}
