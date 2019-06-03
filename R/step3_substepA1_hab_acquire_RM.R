#' Breeding habitat acquisition substep of pre-breeding step of breeding season using original Runge & Marra allocation functions
#'
#' For an alternative individual based (IB) approach see substep_breed_hab_acquire_IB()
#'
#' @param W2 population structure after winter and migration mortality
#' @param K.bc carrying capacity of breeding ground source (._c) habitat
#' @param K.bk carrying capacity (K) of breeding ground skin (._k) habitat
#' @param ... additional arguements
#'
#' @export


step3_substepA1_hab_acquire_RM <- function(W2,
                                      K.bc,# = param.set$K.bc,
                                      K.bk,# = param.set$K.bk
                                      ...){
  ###########################################
  ### Summer Habitat Acquisitiom dynamics ###
  ###########################################

  # These functions all create scalars reprsenting the number of
  # birds by sex in each type of habitat (source vs. sink;
  # excess males become floaters)

  #----------------------------------#
  #   FEMALE (F) Habitat Aquisition  #
  #----------------------------------#

  ### EQUATION 4: eq04_Bfc()
  ### Number of FEMALES (B.fc) in SOURCE (c) habitat

  #females from good winter habitat preferentially acquire source
  #habitat during summer

  B.fc <- eq04_Bfc(W2, K.bc)

  ### EQUATION 5:eq05_Bfk()
  ### Number of Females in SINK (k) habitat

  B.fk <- eq05_Bfk(W2,
                        K.bc,
                        K.bk)

  #Note: excess females leave the system,
  #whereas males become "drain" males
  #(aka floaters)

  #---------------------------------#
  #   MALE (M) Habitat Aquisition   #
  #---------------------------------#

  ### EQUATION 6:
  ### Males acquiring  source habitat

  #males from good winter habitat preferentially acquire source
  #habitat during summer and therefore pair w/females most likely
  #to have wintered in good habitat
  #
  # Note: Density dependence occurs via a ceiling function

  B.mc <-eq06_Bmc(W2,
                        K.bc)

  ### EQUATION 7: eq07_Bmk()
  ### MALES the acquire sink habitat
  B.mk <-  eq07_Bmk(W2,
                        K.bc,
                        B.fk)


  ### EQUATION 8:  eq08_Bmd()
  ### Males that don't find a territory become "drain" males (floaters)

  #   W2["mg"]+W2["mp"] = total male population
  #   minus those that ended up in source habitat (K.bc)
  #   minus those that paired w/female in sink habitat (B.fk)

  B.md <-eq08_Bmd(W2,
                        K.bc,
                        B.fk) #M.2.drain.eq8

  hab.acquire.results.RM <- list(B.fc = B.fc,
                              B.fk = B.fk,
                              B.mc = B.mc,
                              B.mk = B.mk,
                              B.md = B.md)

  return(hab.acquire.results.RM)
}


