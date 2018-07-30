

substep_breed_mate_acquire_RM <- function(W2,
                                          param.set,
                                          hab.acquire.results){
  ###############################
  ### Summer Pairing dynamics ###
  ###############################

  # Equations 9 through 16
  # all output are scalars

  ### EQUATION 9 eq09_Pcgg()

  #browser()
  P.cgg <- eq09_Pcgg(W2,
                     K.bc = param.set$K.bc,
                     B.mc = hab.acquire.results$B.mc,
                     B.fc = hab.acquire.results$B.fc)

  ### EQUATION 10: eq10()
  ### proportion of pairs on source (c) habitat composed of males from good and
  ### female from poor


  P.cgp <- eq10_Pcgp(W2,
                     K.bc = param.set$K.bc,
                     B.fc = hab.acquire.results$B.fc,
                     B.mc = hab.acquire.results$B.mc)



  ### EQUATION 11:  eq11()
  ### Proportion of poor males mated w/ "good" female
  #### NOte: subscripts wrong in original paper

  P.cpg <- eq11_Pcpg(W2 = W2,
                     K.bc = param.set$K.bc,
                     B.fc = hab.acquire.results$B.fc,
                     B.mc = hab.acquire.results$B.mc)




  ### EQUATION 12 eq12_Pcpp()
  ####  proportion composed of a male and female both from poor habitat
  ####  This is calcualted by subtraction
  ####  pairing.eq12.P.cpp <- function(P.cgg,P.cgp, P.cpg){1 - P.cgg - P.cgp - P.cpg}


  #APPLY EQUATION 12
  # pairing.eq12.P.cpp
  P.cpp <- eq12_Pcpp(P.cgg,
                     P.cgp,
                     P.cpg)




  ### EQUATION 13: eq13()
  ### pairing in SIN.K. habitat

  ### pairing.eq13.P.kgg
  P.kgg <- eq13_Pkgg(W2,
                     K.bc = param.set$K.bc,
                     K.bk = param.set$K.bk,
                     B.mk = hab.acquire.results$B.mk,
                     B.fk = hab.acquire.results$B.fk)



  ### EQUATION 14: eq14()
  ### proportion in sink habitat, good-poor pairs

  P.kgp <- eq14_Pkgp(W2 = W2,      #note that both eq14 and eq habve .kgp subscripts in original paper
                     K.bc = param.set$K.bc,
                     K.bk = param.set$K.bk,
                     B.mk = hab.acquire.results$B.mk,
                     B.fk = hab.acquire.results$B.fk)


  ### EQUATION 15: eq15()

  ### note that both eq14 and eq habve .kgp subscripts in original paper
  P.kpg <- eq15_Pkpg(W2,
                     param.set$K.bc,
                     param.set$K.bk,
                     hab.acquire.results$B.mk,
                     hab.acquire.results$B.fk)



  ### EQUATION 16: eq16()
  #   proportion in sink composed of poor-poor

  P.kpp <- eq16_Pkpp(P.kgg, P.kgp, P.kpg,
                     hab.acquire.results$B.mk,
                     hab.acquire.results$B.fk)


  #set up
  P.breeding.pair.results <- c(P.cgg, P.cgp,P.cpg, P.cpp,
                    P.kgg, P.kgp,P.kpg, P.kpp)

  names(P.breeding.pair.results) <- c("P.cgg", "P.cgp","P.cpg", "P.cpp",
                           "P.kgg", "P.kgp","P.kpg",  "P.kpp")


  return(P.breeding.pair.results)


}
