#'
#' @export


substep_competition <- function(B2,
                                Y2,
                                param.set,
                                gamma.i,
                                i,
                                ...){

  ### Equation 23 eq23_stack_Ai() ###
  #### Stack adults &  young into single vector (A)

  A <-eq23_stack_Ai(B2, #Adults after migration
                       Y2) #offspring after migration


  ### Competition for territories
  #NB: breeding K is based on pairs,
  #    winter K is based on individuals!

  #competition ability depends on "an intrsince age-,sex-
  # and condition (habitat)-specific competitive factor
  # gamma & the number of birds in each class" (Runge & Marra 2004, pg xxx)


  #competition should only occur when the number of birds arriving from migration
  #exceeds winter carrying capacity K.wg  This is what is implied by Fig 28.4


  ### build dataframe to hold diagnostic data about competition
  #### <<This step could be turned off to save a little bit of computation>>
  df <- data.frame(j = rep(NA,1000),
                   K.wg.j.init = NA,
                   K.wg.j.end = NA,
                   tot.settled.init = NA,
                   tot.settled.final = NA,
                   tot.active.init = NA,
                   tot.active.final = NA,
                   suc.settled.raw = NA,
                   suc.settled.cor = NA,
                   un.settled = NA)

  ### Number of iterations to run loop to resolve competition
  comp.its <- nrow(df)



  ### Equation 24 and Equation 25#
  #### eq24_comp_loop() implements a loop
  #### that runs eq24_competition() and eq25_comp_constrain()
  #### gamma.i created by eq24_make_gamma_vec()


  # A = stacked pop vector from eq 23

  ## Initial states
  A.i.0 <- A
  K.wg.0 <- param.set$K.wg


  ## Run eq24_comp_loop()
  ### Outputs a list

  comp.out.list <- eq24_comp_loop(A.i.0 = A.i.0,
                                  K.wg.0 = K.wg.0,
                                  y.i = gamma.i,
                                  comp.df = df,
                                  j = comp.its,
                                  i = i)




  #Birds alloacted to good winter habitat
  A.i.G <- comp.out.list$A.i.G.settled.tot.j



  ### Equation 26 Allocated birds to poor winter habitat ###
  ####  A.i.P <- A.i-A.i.G
  A.i.P <- eq26_alloc_winter_P(A.i.0 = A.i.0,
                               A.i.G = A.i.G)


  ### Name output
  class.names <- c("mc","mk","md","fc","fk","y.mc","y.mk","y.fc","y.fk")
  names(A.i.G) <- class.names
  names(A.i.P) <- class.names



  ### EQUATION 27 ###
  # Combine young & old after competition
  # differences between ages and breeding site disappear at this stage
  #

  ###NB: returns W.mg,W.mp,W.fg,W.fp objects to workspace
  #
  W.list <- eq27_post_comp_pooling(A.i.G = A.i.G,
                                   A.i.P = A.i.P)



  #Does output of competition match input before competition?
  temp <- round(sum(A.i.0),4) ==
    round(sum(W.list$W.mg,W.list$W.mp,W.list$W.fg,W.list$W.fp),4)


  if(temp == FALSE){
    #
    #message("ERROR RELATED TO COMPETITION!!!!!!!!!!!!!!!!!1")
  }


  ### Test competition output

  # if(any(A.i.G > A.i.0)){
  #   #message("Competition error in runFAC: A.i.G > A.i.0 on iteration " ,i)
  #   browser()
  # }
  #
  # if(sum(A.i.G) > param.set$K.wg){
  #   #message("Competition error runFAC: sum(A.i.G) > K.wg on iteration ",i," ",
  #           #sum(A.i.G)," vs ",param.set$K.wg)
  #   browser()
  #
  # }
  #
  # if(any(A.i.P) < 0){
  #   #message("Competition error runFAC: any(A.i.P) < 0 on iteration", i, " ")
  #   browser()
  # }



return(W.list)



}

