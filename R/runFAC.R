#' Run FAC model to equilibirum
#'
#' Iterates a single parameterization of the Runge & Marra 2004 FAC model
#' until equilbirium is reached.  Typically the desired output is the equilibrium
#' population size(s) and all intermediate output is discarded.
#'
#' @param iterations how many iterations of model to run before stopping
#' @param Ninit vector of initial abundances at the begining of winter for W.mg, W.mp, W.fg and W.fp
#' @param param.set initila parameters for this run of the model.
#' @param scenario character string representing name of scenario explored in original Runge and Marra paper (not currently implemented 7/10/2018)
#' @param vary ...
#' @param verbose verbose general output
#' @param check.errors Run error checks on model subcomponents
#' @param check.errors.in run error checks and report
#' @param check.eq should equilibrium be assessed?
#' @param minimum.i number of iterations to run if equilibrium is being checked
#' @param diagnostic.plot Return a diagnostic plot show population size over time
#' @param eq.tol Tolerance for equilibirum check; the number of digistic lambda and variace of lambda should be rounded to when comparing against 1.  Larger numbers reduce the likelihood of model passing the equilbirium check
#' @param return.output Output the full model dataframe?
#' @param save.ts Save the full time series of the model run?  If FALSE then only final time point will be returned.  Additionally, equilibrium monitoring will not be done and the time series cannot be plotted
#' @param use.IBM
#' @param ... other arguements passed
#'
#' @return A dataframe containing the status of the popualtion at each time step for all parameters
#'
#'
#' @references Runge, MC & PP Marra 2004.  Modeling seasonal interactions in the population dynamics of migratory birds.  Birds of two words.
#'
#' @examples
#' # Run an FAC
#' test.FAC <- runFAC(verbose = TRUE)
#'
#' # Look at structure of output
#' str(test.FAC,1)
#'
#' @export

runFAC <- function(iterations = 350 #number of generations to run model;
                   ,Ninit = c(10,0,10,0)
                   ,param.set = param_set()
                   ,scenario = NA
                   ,vary = NA
                   ,verbose = FALSE
                   ,check.errors = TRUE
                   ,check.errors.in = c("B0.RM", "P.cgg", "P.cgp","P.cpg", "P.cpp",
                                              "P.kgg", "P.kgp","P.kpg", "P.kpp",
                                               "Y1")

                   ### Diagnostics and misc
                   ,check.eq = TRUE #check for equilibrium
                   ,minimum.i = 20
                   ,eq.tol = 6
                   ,diagnostic.plot = F
                   ,return.output = T
                   ,save.ts = T
                   ,use.IBM = F
                   ,...
                ){

#################################
###     Set model params      ###
###     and do other prelims  ###
#################################

### Set up
#### Dataframe to hold output of each iteration of model
#### matrices holding survival and other parameters
#### output: list
runFAC.i <- runFAC_set_up(param.set,
                          iterations,
                          use.IBM)





#############################
###    Modeling loop      ###
#############################

### Implement model iteratively
#### Iterate model in order to reach stable equilibrium

for(i in 1:iterations){

  #*#*#*#*#*#*#*#*#*#*#*#*#*#
  #*#*#
  #*#*# Winter Dynamics #*#*#
  #*#*#
  #*#*#*#*#*#*#*#*#*#*#*#*#*#

  ##################
  ### EQUATION 1 ###
  ##################

  #### Create vector W0 of population state at end of winter
  ##### (W.xx are the output of the final step of each iteration of the model)


  #### 1st iteration only:
  ##### Initial winter population state

  if(i == 1){

    # Populate list w/ initial population sizes from main function call
    W.list <- list(W.mg = Ninit[1]
                  ,W.mp = Ninit[2]
                  ,W.fg = Ninit[3]
                  ,W.fp = Ninit[4])

    # Create vector W0
    W0.RM <- eq01buildW0vect(W.list$W.mg,
                             W.list$W.mp,
                             W.list$W.fg,
                             W.list$W.fp)


        if(use.IBM == TRUE){
        # Create copy of vector W0 for use with individual based (IB) model
          W0.IB <- W0.RM
        }

    }

  #### All iteractions except the 1st
  if(i > 1){

    W0.RM <- eq01buildW0vect(W.list.RM$W.mg,
                             W.list.RM$W.mp,
                             W.list.RM$W.fg,
                             W.list.RM$W.fp)

        if(use.IBM == TRUE){
          W0.IB <- eq01buildW0vect(W.list.IB$W.mg,
                                   W.list.IB$W.mp,
                                   W.list.IB$W.fg,
                                   W.list.IB$W.fp)

        }
  }



                  ###################
                  ### error check ###
                  ###################

                  # if(check.errors == TRUE & any(W0.RM < 0)){
                  #
                  #   err.msg <- paste("element of W0 < 0 on iteration ",i)
                  #   message(err.msg)
                  #   error.log <- list(param.set = param.set,
                  #                     error.msg = err.msg)
                  #   save(error.log,file = "./error_log/param_set_neg_popsize.RData")
                  #
                  # }



  ###################
  ### EQUATION 2: ###
  ###################

  ### Winter SURVIVAL (S.w) of birds in different habitat qualities
  #### Calculcate Population state at end of winter
  W1.RM <- runFAC.i$param.matrices$S.w %*% W0.RM


      if(use.IBM == T){
        W1.IB <- runFAC.i$param.matrices$S.w %*% W0.IB
      }




  #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
  #*#*#                                             #*#*#
  #*#*# Spring season northward migration Dynamics  #*#*#
  #*#*#                                             #*#*#
  #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

  ###################
  ### EQUATION 3  ###
  ###################

  ### Northward migration survival (S.m)

  #### Calulcate Population at end of northward migration

  W2.RM <- runFAC.i$param.matrices$S.m %*% W1.RM
    names(W2.RM) <- c("mg","mp","fg","fp")


      if(use.IBM == T){
        W2.IB <- runFAC.i$param.matrices$S.m %*% W1.IB
        names(W2.IB) <- c("mg","mp","fg","fp")
      }






  #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
  #*#*#                                  #*#*#
  #*#*#  Breeding season Dynamics        #*#*#
  #*#*#                                  #*#*#
  #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*

  ### Breeding season involves 3 steps
  #### 1) Step 1: Pre-breeding
  #### 2) Step 2: breeding
  #### 3) Step 3: post-breeding


  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
  #-# Breeding Step 1: Pre-breeding       #-#
  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

      ### Pre-breeding is composed of 2 substeps
      ### 1) Step 1: Pre-breeding
      ###         Substep 1a) Habitat acquisition
      ###             Function: substep_B_hab_acquire_RM()
      ###                          Equations: 4 through 8
      ###         Subset 2b) Mate acquisition
      ###             Functions:
      ###                          Equations: 9 through 16

      #--#--#--#--#--#--#--#--#--#--#--#--#--#--#
      #-- Breeding Step 1,
      #--     Substep 1 of 2     --#
      #-- Breeding Habitat Acquisitiom        --#
      #--#--#--#--#--#--#--#--#--#--#--#--#--#--#


      # Runge & Marra habitat acquitions allocation functions
      ## returns list w/ 5 elements, one scaler for each subset of the breeding populations
      hab.acquire.results.RM <- substep_B_hab_acquire_RM(W2.RM,
                                                          K.bc = param.set$K.bc,
                                                          K.bk = param.set$K.bk)


          # Individual-based habitat acquitions
          #    TO DO: add habitat df to be added externally
          #                 to allow habitat to vary continuously in quality
          #debugonce(substep_B_hab_acquire_IB)

          #returns list w/ 6 elements
          #   1st 5 are the 5 subsets of the popualtion
          #   last element is a dataframe with individual based data

          if(use.IBM == TRUE){
            hab.acquire.results.IB <- substep_B_hab_acquire_IB(W2.IB,
                                                               K.bc = param.set$K.bc,
                                                               K.bk = param.set$K.bk)
          }



      #--#--#--#--#--#--#--#--#--#--#--#--#--#--#
      #-- Breeding Step 1
      #--    Substep 2 of 2                   --#
      #-- Breeding season mate Acquisition    --#
      #--#--#--#--#--#--#--#--#--#--#--#--#--#--#


      P.breeding.pair.results.RM <- substep_B_mate_acquire_RM(W2.RM,
                                    param.set,
                                    hab.acquire.results.RM)


          ### Individual based model - mate acquisition
          #### 6th element of list returned by in last step
          #### by substep_B_hab_acquire_IB()
          #### contains the >>full dataframe<< of individual based results
          #### the other 5 elements are the B.xx scalars for comparison
          #### to the standard Runge & Marra approach (".RM")
          if(use.IBM == TRUE){
            P.breeding.pair.results.IB <- substep_B_mate_acquire_IB(hab.acquire.results.IB[[6]])
          }





      ####################################
      ### EQUATION 17: eq17buildVect() ###
      ####################################

      ## Compile results of pairing
      ### vector of results after pairing

      B0.RM <- eq17buildVect(hab.acquire.results.RM$B.mc,
                          hab.acquire.results.RM$B.mk,
                          hab.acquire.results.RM$B.md,
                          hab.acquire.results.RM$B.fc,
                          hab.acquire.results.RM$B.fk)


        if(use.IBM == TRUE){
          B0.IB <- eq17buildVect(hab.acquire.results.IB$B.mc,
                                 hab.acquire.results.IB$B.mk,
                                 hab.acquire.results.IB$B.md,
                                 hab.acquire.results.IB$B.fc,
                                 hab.acquire.results.IB$B.fk)
        }



       #x#x# #x#x# #x#x# #x#x#
       #x#x# ERROR CHECK #x#x#
       #x#x# #x#x# #x#x# #x#x#

       ### Check B0.RM for errors

       if(check.errors == TRUE){
         error_check_summer(B0.RM,
                            W2.RM,
                            P.breeding.pair.results.RM,
                            check.errors.in,
                            i)

            if(use.IBM == T){
              error_check_summer(B0.IB,
                                 W2.IB,
                                 P.breeding.pair.results.IB,
                                 check.errors.in,
                                 i)
            }
         }





    #####################################
    ### Breeding Season Breeding step ###
    #####################################


    ### REPRODUCTION
    ####"average fecunidty for pairs in source and sink habitat"
    #### is a function of how the proportion of pairs in that
    ####  habitat that are good-good, good-poor etc


    ####################
    ### EQUATION 18  ###
    ####################

    ### Load Equation 18
     P.all.RM <- eq18buildPmat(P.breeding.pair.results.RM["P.cgg"],
                            P.breeding.pair.results.RM["P.cgp"],
                            P.breeding.pair.results.RM["P.cpg"],
                            P.breeding.pair.results.RM["P.cpp"],
                            P.breeding.pair.results.RM["P.kgg"],
                            P.breeding.pair.results.RM["P.kgp"],
                            P.breeding.pair.results.RM["P.kpg"],
                            P.breeding.pair.results.RM["P.kpp"])





        if(use.IBM ==TRUE){
          P.all.IB <- eq18buildPmat(P.breeding.pair.results.IB["P.cgg"],
                                 P.breeding.pair.results.IB["P.cgp"],
                                 P.breeding.pair.results.IB["P.cpg"],
                                 P.breeding.pair.results.IB["P.cpp"],
                                 P.breeding.pair.results.IB["P.kgg"],
                                 P.breeding.pair.results.IB["P.kgp"],
                                 P.breeding.pair.results.IB["P.kpg"],
                                 P.breeding.pair.results.IB["P.kpp"])
        }

    ### APPLY EQUATION 18
    R.RM <- P.all.RM%*%runFAC.i$param.matrices$R.all

        if(use.IBM == TRUE){
          R.IB <- P.all.IB%*%runFAC.i$param.matrices$R.all
        }



    #####################################
    ### EQUATION 19 eq19buildMinMat() ###
    #####################################

    ### Prep equation 19
    eq19.min.mat.RM <- eq19buildMinMat(hab.acquire.results.RM$B.mc,
                                        hab.acquire.results.RM$B.fc,
                                        hab.acquire.results.RM$B.mk,
                                        hab.acquire.results.RM$B.fk)



          if(use.IBM == TRUE){
            eq19.min.mat.IB <- eq19buildMinMat(hab.acquire.results.IB$B.mc,
                                            hab.acquire.results.IB$B.fc,
                                            hab.acquire.results.IB$B.mk,
                                            hab.acquire.results.IB$B.fk)

          }


    ### Apply equation 19
    #### Cacluate reproductive output
    Y1.RM <- runFAC.i$param.matrices$f.mat%*%eq19.min.mat.RM%*%R.RM
    names(Y1.RM) <- c("mc","mk","fc","fk")


        if(use.IBM == TRUE){
          Y1.IB <- runFAC.i$param.matrices$f.mat%*%eq19.min.mat.IB%*%R.IB
          names(Y1.IB) <- c("mc","mk","fc","fk")
        }



    #x#x# #x#x# #x#x# #x#x#
    #x#x# ERROR CHECK #x#x#
    #x#x# #x#x# #x#x# #x#x#

    ### TO DO Make this compatible with the "check errors in" arguement
    ###       make into a function
    if(check.errors == TRUE){
      if(any(is.na(Y1.RM)) == TRUE |
         any(is.nan(Y1.RM)) == TRUE){
        message("NAs in Y vector")
      }

    }






    ##############################
    ### Summer adult mortality ###
    ##############################


    ### BREEDING season mortality
    #### "adult birds experienc# both sex- and habitat specific mortality
    ####  over the breeding season.

    #####################
    ### Equation 20:  ###
    #####################

    #### eq20_build_Sb_mat() builds the S.b matrix
    B1.RM <- runFAC.i$param.matrices$S.b%*%B0.RM



        if(use.IBM == TRUE){
          B1.IB <- runFAC.i$param.matrices$S.b%*%B0.IB
        }


    #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*##*#*#
    #*#*#                          #*#*#
    #*#*# Fall migration  Dynamics #*#*#
    #*#*#                          #*#*#
    #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*##*#*#

    ###########################
    ### MIGRATION Mortality ###
    ###########################

    #---------------------------------#
    #   MIGRATION Mortality of adults #
    #---------------------------------#


    ### "Mort during mig depends upon the sex of the bird &
    ### breeding habitat it used"

    ######################
    ### EQUATION 21:   ###
    ######################

    ### eq21_build_Sf_mat() builds S.f

    B2.RM <- runFAC.i$param.matrices$S.f%*%B1.RM
      names(B2.RM) <- c("mc","mk","md","fc","fk")


        if(use.IBM == TRUE){

          B2.IB <- runFAC.i$param.matrices$S.f%*%B1.IB
              names(B2.IB) <- c("mc","mk","md","fc","fk")

        }


    #------------------------------------#
    #   MIGRATION Mortality of young (y) #
    #------------------------------------#


    ### EQUATION 22 eq22() ###
    Y2.RM <- runFAC.i$param.matrices$S.y%*%Y1.RM
        names(Y2.RM) <- c("y.mc","y.mk","y.fc","y.fk")

        if(use.IBM==TRUE){
          Y2.IB <- runFAC.i$param.matrices$S.y%*%Y1.IB
          names(Y2.IB) <- c("y.mc","y.mk","y.fc","y.fk")
        }




    #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
    #*#*#                             #*#*#
    #*#*# Winter competitiom Dynamics #*#*#
    #*#*#                             #*#*#
    #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#


    W.list.RM <- substep_competition(B2.RM,
                                  Y2.RM,
                                  param.set,
                                  gamma.i = runFAC.i$param.matrices$gamma.i,
                                  i)


        if(use.IBM == TRUE){
          W.list.IB <- substep_competition(B2.IB,
                                           Y2.IB,
                                           param.set,
                                           gamma.i = runFAC.i$param.matrices$gamma.i,
                                           i)
        }


    #browser()



          ############################################
          ### Save info for diagnosing equilibrium
          ############################################

          ## this could be turned in to a small function

          ### NOTE: currently use -RM, NOT IBM

          # Save current number of winter male pop size in good habitat
          runFAC.i$W.mg.diagnostic.df[i,"W.mg"] <- W.list.RM$W.mg

          #calcualte lambda for current time step
          if(i>1){
            W.mg.t2 <- runFAC.i$W.mg.diagnostic.df[i,"W.mg"]
            W.mg.t1 <- runFAC.i$W.mg.diagnostic.df[i-1,"W.mg"]

            runFAC.i$W.mg.diagnostic.df[i,"W.mg.lambda"] <- W.mg.t2/W.mg.t1
          }




          # this could be turned into a small function
          #range of values over which to do diagnostics for lambda
          ## use last 10 time steps to calcualte mean lambda
          if(i > 10){
            lamb.diagnose.range.i <-c(i-10):i
            diagnostic.lambs <- runFAC.i$W.mg.diagnostic.df[lamb.diagnose.range.i,"W.mg.lambda"]


            #calcualte mean and var of labmda
            runFAC.i$W.mg.diagnostic.df[i,"W.mg.lambda.mean"] <- mean(diagnostic.lambs, na.rm = T)
            runFAC.i$W.mg.diagnostic.df[i,"W.mg.lambda.var"]  <- var(diagnostic.lambs, na.rm = T)

          }



          ##############################
          ###
          ### Check for model completion
          ###
          ##############################

          ###########################
          ### Check for equilibiurm
          ###########################

          at.eq <- FALSE
          if(check.eq == TRUE & i > minimum.i){
            at.eq <- runFAC_check_equilibrium(runFAC.i$W.mg.diagnostic.df[i,"W.mg.lambda.mean"],
                                              runFAC.i$W.mg.diagnostic.df[i,"W.mg.lambda.var"],
                                              i,
                                              at.eq = FALSE,
                                              verbose = TRUE,
                                              eq.tol,
                                              ...)
          }



    #############################################
    ### Save full output of model each iteration
    #############################################


    ###  pick up everything from environment & save
    #### NOTE: this could probably be made more efficient
    ####       right now it picks up entire df frame, adds data to current
    ####       column i, then outputs entire df to workspace
    #### Also, to speed things up I should

    ### Save current state in real time ##
    #### for saving full time series (ts)
    if(save.ts == TRUE){


      ## Save Runge & Marra equations
        runFAC.i$FAC.out.RM <- runFAC_store_pop_state_i(i,
                                           minimum.i,
                                           runFAC.i$FAC.out.RM,
                                 W.list.RM,
                                 B0.RM,
                                 P.breeding.pair.results.RM,
                                 Y2.RM)

          ## Save IMB if run
          if(use.IBM==T){
            runFAC.i$FAC.out.IB <- runFAC_store_pop_state_i(i,
                                               minimum.i,
                                     runFAC.i$FAC.out.IB,
                                     W.list.IB,
                                     B0.IB,
                                     P.breeding.pair.results.IB,
                                     Y2.IB)
          }
     }


    ### Save equilibirum state
    #### Equilibirum state saved to seperate slot of output list
    #### for easy access
    if(i == iterations | at.eq == TRUE){
      runFAC.i$FAC.eq.state.RM <- runFAC_store_pop_state_i(i,
                                          minimum.i,
                                          runFAC.i$FAC.out.RM,
                                         W.list.RM,
                                         B0.RM,
                                         P.breeding.pair.results.RM,
                                         Y2.RM)[i,]


          if(use.IBM == T){
            runFAC.i$FAC.eq.state.IB <- runFAC_store_pop_state_i(i,
                                                                 minimum.i,
                                                                 runFAC.i$FAC.out.IB,
                                                                 W.list.RM,
                                                                 B0.IB,
                                                                 P.breeding.pair.results.IB,
                                                                 Y2.IB)[i,]
          }

     }


    ######################################
    ### Break loop loop if at equilibrium
    ######################################

    if(check.eq == TRUE & at.eq == TRUE){
      break
    }






  }#close main for() loop for iterating model



  ########################
  ### Finalize output  ###
  ########################


  ### Add meta data on model run ###
  FAC.meta <- list(
    check.eq = check.eq,
    i.final = i,
    max.iterations.set.at = iterations,
    min.iterations.set.at = minimum.i,
    use.IBM = use.IBM,
    Ninit = Ninit,
    save.ts = save.ts,
    lambda.mean = runFAC.i$W.mg.diagnostic.df[i,"W.mg.lambda.mean"],
    lambda.var = runFAC.i$W.mg.diagnostic.df[i,"W.mg.lambda.var"])



  ### Finalize full dataframe of output
  #### Total up seasonal population sizes
  #### round off numbers
  #### etc


  ### Finalize main datafrme
  #### main df of time series output
  if(save.ts == TRUE){
    runFAC.i$FAC.out.RM <- runFAC_finalize_output(runFAC.i$FAC.out.RM)

    if(use.IBM == TRUE){
      runFAC.i$FAC.out.IB <- runFAC_finalize_output(runFAC.i$FAC.out.IB)

    }
  }







  ### Finalize equlibrium state
  #### Finalize the element in list that just contains the final sate

  ### If full time series being save
  if(save.ts == TRUE){
    runFAC.i$FAC.eq.state.RM <- runFAC_finalize_output(runFAC.i$FAC.out.RM,
                                                    save.ts = TRUE)[i,]

    if(use.IBM == TRUE){
      runFAC.i$FAC.eq.state.IB <- runFAC_finalize_output(runFAC.i$FAC.out.IB,
                                                         save.ts = TRUE)[i,]
    }
  }

  ### If full time series not being save
  ###  (why seperate?)

  if(save.ts == FALSE){
    runFAC.i$FAC.eq.state.RM <- runFAC_finalize_output(runFAC.i$FAC.eq.state.RM,
                                                    save.ts = FALSE)

    if(use.IBM == TRUE){
      runFAC.i$FAC.eq.state.IB <- runFAC_finalize_output(runFAC.i$FAC.eq.state.IB,
                                                      save.ts = FALSE)
    }
  }



  ### Plot Diagnostic for full run of model
  if(save.ts == TRUE & diagnostic.plot == T){
    plot_runFAC(runFAC.i$FAC.out)
  }


  ### Return all output
  if(return.output == TRUE){
    return(runFAC.i)
  }


}
