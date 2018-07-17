#' Equation 11:
#'
#' @details
#' SUMMER MALE (M) DYNAMICS
#' Aliased as pairing.eq11.P.cpg()
#'   proportion of poor males mated w/ "good" female
#'   1)If males that wintered in good habitat > females that wintered in good habitat
#'     BUT males that wintered in good less than source K
#'     Then the proportion of poor-good pairings is
#'     a function of
#'   Otherwise, there are no poor-good pairings
#'
#' Previously aliased as pairing.eq11.P.cpg
#'
#' @param W2 population vector; indicats where birds are coming from
#' @param K.bc females  sour.c.habitat carrying capacity
#' @param B.fc females alreadiy in source
#' @param B.mc males already in source
#'
#' @return P.cgp, the proportion of pairings in the source habitat made up of xxx
#' from good winter habitat and yyy from poor winter habitat.
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal
#'       interactions in the population dynamics of migratory birds.
#'       In Greenberg, R and PP Marra, eds.  Birds of two worlds.
#'
#' @examples
#' # Test loop
#' ## Values for population vector W2
#' seqW <- seq(1,100,30)
#'
#' ## Values for carrying capacities
#' seqK <- seq(1,125,50)
#'
#' ## dataframe of parameter combinations
#' df <- expand.grid(mg = seqW,
#'                   mp = seqW,
#'                   fg = seqW,
#'                   fp = seqW,
#'                   K.bc = seqK,
#'                   K.bk = seqK,
#'                   P.cgg = NA)
#'
#' ## Loop over all param combos
#' for(i in 1:nrow(df)){
#'     W2 <- df[i,c("mg","mp","fg","fp")]
#'     params <- df[i,c("K.bc","K.bk")]
#'
#'     B.mc <- eq06_Bmc(W2, params$K.bc)
#'     B.fc <- eq04_Bfc(W2, params$K.bc)
#'     df$P.cgg[i] <- eq11_Pcgp(W2 = W2,
#'                              K.bc = params$K.bc,
#'                              B.fc = B.fc,
#'                              B.mc = B.mc)
#' }
#'
#' hist(df$P.cgg)
#'
#' @export


eq11_Pcgp <- function(W2,
                      K.bc,
                      B.fc,
                      B.mc){# K

  #part B of equation 11 (lower part)
  P.cgp <- 0

  #part A of equation 11 (lower part)
  if(W2["mg"] < W2["fg"]){  #original: W2["mg"] > W2["fg"]
    if(W2["mg"] < K.bc){
      num <-  (min( unlist(W2["fg"]), unlist(B.mc)) - unlist(W2["mg"]))
      denom <- min( unlist(B.mc),     unlist(B.fc))

      P.cgp <- num/denom

    }

  }

  #Check and throw messages if problems
  if(is.nan(unlist(P.cgp)) == TRUE){
    message("ERROR IN EQUATION 11: NaN!!")
  }

  if(P.cgp < 0){
    message("ERROR IN EQUATION 11: P < 0!!")
  }

  return(P.cgp)
}
