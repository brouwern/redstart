#' Save current population state
#'
#'
#' @param i current iteration of model
#' @param minimum.i Minimum number of iterations over which to run model
#' @param FAC.out dataframe holding output of model for each iteraction
#' @param W.list list containing state of population after winter and fall migration
#' @param B0 vector of....
#' @param P.breeding.pair.results vector representing pairing frequency of birds in source (".c") habitat the both originated from good (".gg") habitat on the wintering ground.
#' @param Y2 vector of reproductive output (post migration?)
#' @param check.eq Should lambda be calculated
#' @param ... Additional arguements
#'
#' @return FAC.out dataframe updated with population sizeds from time i
#'
#' @export
#'


runFAC_store_pop_state_i <- function(i,
                             minimum.i = 10,
                             FAC.out,
                             W.list,
                             B0,
                             P.breeding.pair.results,
                             Y2,
                             check.eq = T,
                              ...){


  ##############################
  ###
  ### SAVE populat status    ###
  ###
  ##############################

  ##############################
  ### SAVE POPULATION STATES ###
  ##############################


  ### Save winter state ###

  FAC.out[i,c("W.mg","W.mp","W.fg","W.fp")] <- c(W.list$W.mg,
                                                W.list$W.mp,
                                                W.list$W.fg,
                                                W.list$W.fp)


  ### Save BReeding season state ###

  ## Runge & Marra allocation equations
  FAC.out[i,c("B.mc","B.mk","B.md","B.fc","B.fk")]               <- B0




  ### Save offspring ###
  FAC.out[i, c("y.mc","y.mk","y.fc","y.fk")] <- Y2



  ########################################
  ### Save pairing frequencies (P.xxx) ###
  ########################################

  ## Runge & Marra pairing equations
  FAC.out[i, c("P.cgg","P.cgp","P.cpg","P.cpp",
              "P.kgg","P.kgp","P.kpg","P.kpp")]             <- P.breeding.pair.results


  ###########################################
  ### Save winter competition information ###
  ###########################################

  # Store number of birds allocated to G and P habitat
  #  removed



  return(FAC.out)
}


