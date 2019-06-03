#' Equation 11: Proportion of males from poor winter habitat mated with females from good winter habitat
#'
#' @details
#' Proportion of winter-poor males mated w/ winter-goods females
#' 1)If males that wintered in good habitat > females that wintered in good habitat
#'     BUT males that wintered in good less than source K
#'     Then the proportion of poor-good pairings is
#'     a function of
#'   Otherwise, there are no poor-good pairings
#'
#' Note that in the original paper, both equations 10 and 11 have the same subscripts of P.cgp, while in equation 12 it is implied they have different subscripts.
#' It appears that the 2nd subscript should indicate the winter habitat for males and the 3rd should indicate the winter habitat for females.
#' Therefore equation 10 in the original paper remains as P.cgp (P.source.male-good.female-poor) and equation 11 should be changed to P.c.pg (P.source.male-poor.female-good)
#'
#' Note  W2.mg > W2.fg in original paper.  ">" needs to be "<"  b/c poor males end up
#' with good female when there are not enough good males for all the good females (Wmg < Wfg)
#'
#' @param W2 population vector; indicats where birds are coming from
#' @param K.bc females  sour.c.habitat carrying capacity
#' @param B.mc males already in source
#' @param B.fc females alreadiy in source
#' @param ... xxx
#'
#' @return P.cpg, the proportion of pairings in the source habitat (.c _ _)
#' made up of males from poor winter habitat (.cp_) and females from good
#' winter habitat (.c_g).  See note above about how subscripts are wrong in original
#' paper.
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal
#'       interactions in the population dynamics of migratory birds.
#'       In Greenberg, R and PP Marra, eds.  Birds of two worlds.
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
  if(W2["mg"] < W2["fg"]){  # W2["mg"] > W2["fg"] in paper.  ">" needs to be "<"  b/c poor males end up with good female when there are not enough good males for all the good females (Wmg < Wfg)

    if(W2["mg"] < K.bc){
      num <-  (min( unlist(W2["fg"]), unlist(B.mc)) - unlist(W2["mg"]))
      denom <- min( unlist(B.mc),     unlist(B.fc))

      P.cpg <- num/denom

    }

  }



  return(P.cpg)
}
