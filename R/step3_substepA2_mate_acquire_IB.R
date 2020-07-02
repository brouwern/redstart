#' FAC-IBM Step 3, Substep A2: Mate acquition
#'
#' @param hab.aq.results.IB Output from individuals-based model
#'
#' @return P.breeding.pair.results.list.IB
#'
#' @export

step3_substepA2_mate_acquire_IB <- function(hab.aq.results.IB){

  #create consolidated pairing code
  hab.aq.results.IB$pair2 <- NA

  hab.aq.results.IB$pair2[which(hab.aq.results.IB$pair == "bc-mg-fg")] <- "cgg"
  hab.aq.results.IB$pair2[which(hab.aq.results.IB$pair == "bk-mg-fg")] <- "kgg"
  hab.aq.results.IB$pair2[which(hab.aq.results.IB$pair == "bc-mg-fp")] <- "cgp"
  hab.aq.results.IB$pair2[which(hab.aq.results.IB$pair == "bk-mg-fp")] <- "kgp"

  hab.aq.results.IB$pair2[which(hab.aq.results.IB$pair == "bc-mp-fg")] <- "cpg"
  hab.aq.results.IB$pair2[which(hab.aq.results.IB$pair == "bk-mp-fg")] <- "kpg"
  hab.aq.results.IB$pair2[which(hab.aq.results.IB$pair == "bc-mp-fp")] <- "cpp"
  hab.aq.results.IB$pair2[which(hab.aq.results.IB$pair == "bk-mp-fp")] <- "kpp"

  hab.aq.results.IB$pair2 <- factor(hab.aq.results.IB$pair2)


  #total number paired
  paired.tot.source <- length(grep("^[c]",hab.aq.results.IB$pair2))
  paired.tot.sink   <- length(grep("^[k]",hab.aq.results.IB$pair2))

  N.cgg <- length(which(hab.aq.results.IB$pair2 == "cgg"))
  N.kgg <- length(which(hab.aq.results.IB$pair2 == "kgg"))
  N.cgp <- length(which(hab.aq.results.IB$pair2 == "cgp"))
  N.kgp <- length(which(hab.aq.results.IB$pair2 == "kgp"))
  N.cpg <- length(which(hab.aq.results.IB$pair2 == "cpg"))
  N.kpg <- length(which(hab.aq.results.IB$pair2 == "kpg"))
  N.cpp <- length(which(hab.aq.results.IB$pair2 == "cpp"))
  N.kpp <- length(which(hab.aq.results.IB$pair2 == "kpp"))



  N.paired.vec <- c(N.cgg,
                    N.cgp, # g-p
                    N.cpg,
                    N.cpp,

                    N.kgg,
                    N.kgp, # g- p
                    N.kpg,
                    N.kpp)

  P.vec.source <- N.paired.vec[1:4]/paired.tot.source
  P.vec.sink   <- N.paired.vec[5:8]/paired.tot.sink

  P.vec.source[is.nan(P.vec.source)] <- 0
  P.vec.sink[is.nan(P.vec.sink)] <- 0

  P.vec <- c(P.vec.source, P.vec.sink)


  P.breeding.pair.results.IB <- c(P.cgg = P.vec[1],
                                     P.cgp = P.vec[2],
                                     P.cpg = P.vec[3] ,
                                     P.cpp = P.vec[4] ,
                                     P.kgg = P.vec[5] ,
                                     P.kgp = P.vec[6] ,
                                     P.kpg = P.vec[7] ,
                                     P.kpp = P.vec[8] )

  P.breeding.pair.results.list.IB <- list(P = P.breeding.pair.results.IB,
                                     df = hab.aq.results.IB)

  return(P.breeding.pair.results.list.IB)

}
