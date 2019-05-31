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
#' @param use.IBM use individual based model for habitat and mate acquisition
#' @param use.IBM.S.b use IBM for summer adult survival
#' @param ... other arguements passed
#'
#' @return A dataframe containing the status of the popualtion at each time step for all parameters
#'
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal
#'       interactions in the population dynamics of migratory birds.
#'       In Greenberg, R and PP Marra, eds.  Birds of two worlds.
#'       Johns Hopkins University Press, Baltimore.
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
                   ,use.IBM.S.b = F
                   ,...
                ){

#################################
###     Step 0:
###     Set model params      ###
###     and do other prelims  ###
#################################

### Set up
#### Dataframe to hold output of each iteration of model
#### matrices holding survival and other parameters
#### output: list
runFAC.i <- step0_set_up(param.set = param.set,
                          iterations = iterations,
                          use.IBM = use.IBM,
                          use.IBM.S.b = use.IBM.S.b)

   # If any part of IBM is set up,
   # all up stream IBM steps need to also
   # be run
   use.IBM <- runFAC.i$use.IBM



#############################
###    Modeling loop      ###
#############################

### Implement model iteratively
#### Iterate model in order to reach stable equilibrium

for(i in 1:iterations){

  #*#*#*#*#*#*#*#*#*#*#*#*#*#
  #*#*#
  #*#*# Step 1:
  #*#*# Winter Dynamics #*#*#
  #*#*#
  #*#*#*#*#*#*#*#*#*#*#*#*#*#


  ##################
  ### EQUATION 1 ###
  ##################

  #### Create vector W0 of population state at end of winter
  ##### (W.xx are the output of the final step of each iteration of the model)

      #### 1st iteration only:
      ##### Initial winter population state from runFAC() function call


        ### To do: error check to make sure negative abundances have not
        ###        been created

  # W.list.xx objects are created at end of each annual cycle
  ## Set to NULL if its the 1st iteraction
  if(i == 1){
    W.list.RM <- NULL
    W.list.IB <- NULL
  }

  W1.list <- step1_winter_survival(i = i,
                                   runFAC.i = runFAC.i,
                                   use.IBM = use.IBM,
                                   Ninit = Ninit,
                                   W.list.RM = W.list.RM,
                                   W.list.IB = W.list.IB)





  #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
  #*#*#                                             #*#*#
  #*#*# Step 2:                                     #*#*#
  #*#*# Spring season northward migration Dynamics  #*#*#
  #*#*#                                             #*#*#
  #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

  ###################
  ### EQUATION 3  ###
  ###################

  ### Northward migration survival (S.m)

  #### Calulcate Population at end of northward migration

  ### TO DO: create function called step_2_spring_survival

  W2.RM <- runFAC.i$param.matrices$S.m %*% W1.list$W1.RM
    names(W2.RM) <- c("mg","mp","fg","fp")


      if(use.IBM == T){
        W2.IB <- runFAC.i$param.matrices$S.m %*% W1.list$W1.IB
        names(W2.IB) <- c("mg","mp","fg","fp")
      }






  #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
  #*#*#                                  #*#*#
  #*#*#  Step 3:                         #*#*#
  #*#*#  Breeding season Dynamics        #*#*#
  #*#*#                                  #*#*#
  #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*

  ### Breeding season involves 3 substeps
  #### A) Step 1: Pre-breeding
  #### B) Step 2: breeding
  #### C) Step 3: post-breeding


  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
  #-# Breeding Substep A: Pre-breeding       #-#
  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

      ### Pre-breeding is composed of 2 sub-substeps
      ### Substep A: Pre-breeding
      ###         Substep A1) Habitat acquisition
      ###             Function: substep_B_hab_acquire_RM()
      ###                          Equations: 4 through 8, & 17
      ###         Subset A2) Mate acquisition
      ###             Functions:
      ###                          Equations: 9 through 16

      #--#--#--#--#--#--#--#--#--#--#--#--#--#--#
      #-- Breeding Substep A1,
      #-- Breeding Habitat Acquisitiom        --#
      #--#--#--#--#--#--#--#--#--#--#--#--#--#--#


      # Runge & Marra habitat acquitions allocation functions
      ## returns list w/ 5 elements, one scaler for each subset of the breeding populations

      hab.acquire.results.RM <- step3_substepA1_hab_acquire_RM(W2.RM,
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
            hab.acquire.results.IB <- step3_substepA1_hab_acquire_IB(W2.IB,
                                                               K.bc = param.set$K.bc,
                                                               K.bk = param.set$K.bk)
          }




      ######################################
      ### EQUATION 17: eq17buildB0Vect() ###
      ######################################

      ## Compile results of habitat acquition
      ## NOTE: this step has be re-ordered from original paper
      ##       to place all steps related to habitat acquisition
      ##       in sequence in the code


      B0.RM <- eq17buildB0Vect(hab.acquire.results.RM$B.mc,
                               hab.acquire.results.RM$B.mk,
                               hab.acquire.results.RM$B.md,
                               hab.acquire.results.RM$B.fc,
                               hab.acquire.results.RM$B.fk)


      if(use.IBM == TRUE){
        B0.IB <- eq17buildB0Vect(hab.acquire.results.IB$B.mc,
                                 hab.acquire.results.IB$B.mk,
                                 hab.acquire.results.IB$B.md,
                                 hab.acquire.results.IB$B.fc,
                                 hab.acquire.results.IB$B.fk)
      }







      #--#--#--#--#--#--#--#--#--#--#--#--#--#--#
      #-- Breeding Substep A2,
      #-- Breeding season mate Acquisition    --#
      #--#--#--#--#--#--#--#--#--#--#--#--#--#--#




      P.breeding.pair.results.RM <- step3_substepA2_mate_acquire_RM(W2.RM,
                                    param.set,
                                    hab.acquire.results.RM)


          ### Individual based model - mate acquisition
          #### 6th element of list returned by in last step
          #### by substep_B_hab_acquire_IB()
          #### contains the >>full dataframe<< of individual based results
          #### the other 5 elements are the B.xx scalars for comparison
          #### to the standard Runge & Marra approach (".RM")
          if(use.IBM == TRUE){
            hab.acq.IB.df <- hab.acquire.results.IB[[6]]
            P.breed.pair.list.IB <- step3_substepA2_mate_acquire_IB(hab.acq.IB.df)

            P.breeding.pair.results.IB <- P.breed.pair.list.IB$P
            }







      #x#x# #x#x# #x#x# #x#x#
      #x#x# Step 3
      #x#x# Substep A
      #x#x# ERROR CHECK #x#x#
      #x#x# #x#x# #x#x# #x#x#

      ### Check B0.RM and P for errors

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
    ### Step
    ### Substep B
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
     P.all.RM <- eq18BbuildPmat(P.breeding.pair.results.RM["P.cgg"],
                            P.breeding.pair.results.RM["P.cgp"],
                            P.breeding.pair.results.RM["P.cpg"],
                            P.breeding.pair.results.RM["P.cpp"],
                            P.breeding.pair.results.RM["P.kgg"],
                            P.breeding.pair.results.RM["P.kgp"],
                            P.breeding.pair.results.RM["P.kpg"],
                            P.breeding.pair.results.RM["P.kpp"])





        if(use.IBM ==TRUE){
          P.all.IB <- eq18BbuildPmat(P.breeding.pair.results.IB["P.cgg"],
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
    eq19.min.mat.RM <- eq19BbuildMinMat(hab.acquire.results.RM$B.mc,
                                        hab.acquire.results.RM$B.fc,
                                        hab.acquire.results.RM$B.mk,
                                        hab.acquire.results.RM$B.fk)



          if(use.IBM == TRUE){
            eq19.min.mat.IB <- eq19BbuildMinMat(hab.acquire.results.IB$B.mc,
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

        # experimental code for IBM
        # if(use.IBM.S.b == TRUE){
        #   browser()
        #   P.breeding.pair.results.list.IB$df
        # }


    #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*##*#*#
    #*#*#                          #*#*#
    #*#*# Step 4:                  #*#*#
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
    #*#*# Step 5:                     #*#*#
    #*#*# Winter habitat acqusition / #*#*#
    #*#*# competitiom Dynamics        #*#*#
    #*#*#                             #*#*#
    #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#


    W.list.RM <- step5_winter_habitat_acquisition(B2.RM,
                                  Y2.RM,
                                  param.set,
                                  gamma.i = runFAC.i$param.matrices$gamma.i,
                                  i)

    #set W.list.IB to NULL in case not being used
    W.list.IB <- NULL
        if(use.IBM == TRUE){
          W.list.IB <- step5_winter_habitat_acquisition(B2.IB,
                                           Y2.IB,
                                           param.set,
                                           gamma.i = runFAC.i$param.matrices$gamma.i,
                                           i)
        }





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
            diagnostic.lambs <- runFAC.i$W.mg.diagnostic.df[lamb.diagnose.range.i,
                                                            "W.mg.lambda"]


            #calcualte mean and var of labmda
            runFAC.i$W.mg.diagnostic.df[i,"W.mg.lambda.mean"] <- mean(diagnostic.lambs,
                                                                      na.rm = T)
            runFAC.i$W.mg.diagnostic.df[i,"W.mg.lambda.var"]  <- var(diagnostic.lambs,
                                                                     na.rm = T)

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
            at.eq <- runFAC_check_equilibrium(runFAC.i$W.mg.diagnostic.df[i,
                                                                          "W.mg.lambda.mean"],
                                              runFAC.i$W.mg.diagnostic.df[i,
                                                                          "W.mg.lambda.var"],
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
    message("Diagnostic plot build using runFAC.i$FAC.out.RM")
    plot_runFAC(runFAC.i$FAC.out.RM)
  }


  ### Return all output
  if(return.output == TRUE){
    return(runFAC.i)
  }


}
