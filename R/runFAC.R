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
#' @param check.errors.in run error checks and report
#' @param check.eq should equilibrium be assessed?
#' @param minimum number of iterations to run if equilibrium is being checked
#' @param tol.1 tolerance value for diagnostics checks for whether the run of the model has reached equilibrium.  Lower values are more stringent; minimum is 1, which makes the model run the full number of iterations
#' @param tol.2 secondary tolerance value for checking equilbrium; LARGER values increase stringency
#' @param diagnostic.plot Return a diagnostic plot show population size over time
#' @param return.output Output the full model dataframe?
#' @param save.ts Save the full time series of the model run?  If FALSE then only final time point will be returned.  Additionally, equilibrium monitoring will not be done and the time series cannot be plotted
#' @param ... other arguements passed
#'
#' @return A dataframe containing the status of the popualtion at each time step for all parameters
#'
#'
#' @references Runge, MC & PP Marra 2004.  Modeling seasonal interactions in the population dynamics of migratory birds.  Birds of two words.
#' @export

runFAC <- function(iterations = 350 #number of generations to run model; setting tol.1 to a value greater than 1 causes model to check for equilbrium; deafult of tol.1 is 3, which means models check for equilbrium after 1/3 of the iterations have been run
                   ,Ninit = c(10,0,10,0)
                   ,param.set = param_set()
                   ,scenario = NA
                   ,vary = NA
                   ,verbose = FALSE
                   ,check.errors.in = c("B0", "P.cgg", "P.cgp","P.cpg", "P.cpp",
                                              "P.kgg", "P.kgp","P.kpg", "P.kpp",
                                        "Y1")


                   ### Diagnostics and misc
                   ,check.eq = TRUE #check for equilibrium
                   ,check.eq.after.i = 20
                   ,tol.1 = 3 # tolerance value for checking for equlibrium; lower values are more stringed; minimum is 1, which makes the model run the full number of iterations
                   ,tol.2 = 3 #2ndary tolerance value for checking equilbrium; LARGER values increase stringency
                   ,diagnostic.plot = T
                   ,return.output = T
                   ,save.ts = T
                   ,...
                ){


## Warnings

# message("NOTE: equation 9 has been set to overwrite P > 1 errors!!!!!")
# message("\nNOTE: equation 10 has been set to overwrite P > 1 errors!!!!!")
# message("\nNOTE: equation 12 has been set to overwrite P > 1 errors!!!!!")



# Set up parametres

# Check main parameter dataframe
## Check that dataframe of parameters is correct size
QAQC_param_set(param.set)


## Create dataframe to store output from each iteration
out.df <- make_FAC_df(iterations)



## Set up fixed parameters
### Calculate Competition for winter territories: gamma
gamma.i <- with(param.set,
                eq24_make_gamma_vec(gamma))








### EQUATION 2: winter survival matrix
### Winter SURVIVAL (S.w) of birds in different habitat qualities
##### (Former Alias: Fx.make.winter.surv.eq2())
S.w <- with(param.set,
            eq02bulidMat(S.w.mg,
                         S.w.mp,
                         S.w.fg,
                         S.w.fp))

### EQUATION 3: spring migration survival matrix
### Northward migration survival (S.m)
#### (Alias: Fx.make.spring.mig.surv.eq3)
S.m <- with(param.set,
            eq03(S.m.mg,
                 S.m.mp,
                 S.m.fg,
                 S.m.fp))


### Equation 20: Breeding season survival matrix
### (alias: Fx.make.breeding.s.matrix.eq20)
S.b <- with(param.set,
            eq20_build_Sb_mat(S.b.mc,
                 S.b.mk,
                 S.b.md,
                 S.b.fc,
                 S.b.fk))


#EQUATION 21: Spring migration survival matrix = adults
# alias: Fx.make.fall.adult.s.matrix.eq21
S.f <-  with(param.set,
             eq21_build_Sf_mat(S.f.mc,
                  S.f.mk,
                  S.f.md,
                  S.f.fc,
                  S.f.fk))



### EQUATION 22 Spring mgiraiton survival matrix - young
# alias: Fx.make.fall.adult.s.matrix.eq22
S.y <- with(param.set,
            eq22_build_Sy_mat(S.y.mc,
                 S.y.mk,
                 S.y.fc,
                 S.y.fk))











## Implement model iteratively
### Iterate model in order to reach stable equilibrium
for(i in 1:iterations){

  # 1st iteration only:
  ## Initial winter population state
  ## (W.xx are the output of the final step of each iteraction of the model)
  if(i == 1){
    W.list <- list(W.mg = Ninit[1]
      ,W.mp = Ninit[2]
      ,W.fg = Ninit[3]
      ,W.fp = Ninit[4])
    }

  #*#*#*#*#*#*#*#*#*#*#*#*#*#
  #*#*#
  #*#*# Winter Dynamics #*#*#
  #*#*#
  #*#*#*#*#*#*#*#*#*#*#*#*#*#

  ### EQUATION 1
  #### Create vector of population state
  W0 <- eq01buildW0vect(W.list$W.mg, W.list$W.mp,
                        W.list$W.fg, W.list$W.fp)

  #if(i == 37){ browser() }

  if(any(W0 < 0)){
    err.msg <- paste("element of W0 < 0 on iteration ",i)
    message(err.msg)
    error.log <- list(param.set = param.set,
                      error.msg = err.msg)
    save(error.log,file = "./error_log/param_set_neg_popsize.RData")

  }


  ### EQUATION 2:
  ### Winter SURVIVAL (S.w) of birds in different habitat qualities

  #### Calculcate Population state at end of winter
  W1 <- S.w%*%W0


  #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
  #*#*#
  #*#*# Spring northward migration Dynamics #*#*#
  #*#*#
  #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

  ### EQUATION 3
  ### Northward migration survival (S.m)

  #### Calulcate Population at end of northward migration
  W2 <- S.m%*%W1; names(W2) <- c("mg","mp","fg","fp")



  #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
  #*#*#                                  #*#*#
  #*#*# Summer breeding season  Dynamics #*#*#
  #*#*#                                  #*#*#
  #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*


  ###########################################
  ### Summer Habitat Acquisitiom dynamics ###
  ###########################################

  # These functions all create scalars reprsenting the number of
  # birds by sex in each type of habitat (source vs. sink;
  # excess males become floaters)

  #----------------------------------#
  #   FEMALE (F) Habitat Aquisition  #
  #----------------------------------#

  ### EQUATION 4: eq04()
  ### Number of FEMALES (B.fc) in SOURCE (c) habitat
  #### (alias #F.2.source.eq4.c)

  #females from good winter habitat preferentially acquire source
  #habitat during summer

  #scalar output
  B.fc <- with(param.set,
               eq04_Bfc(W2,
                        K.bc))

  ### EQUATION 5: eq05calcScalar()
  ### Number of Females in SINK (k) habitat
  #### (Alias F.2.sink.eq5.k)

  #scalar output
  B.fk <- with(param.set,
               eq05_Bfk(W2,
                        K.bc,
                        K.bk))

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


  # scalar out
  # (alias #M.2.source.eq6.c)
  B.mc <- with(param.set,
               eq06_Bmc(W2,
                      K.bc))

  ### EQUATION 7: eq07calcScalar()
  ### MALES the acquire sink habitat
  # (alias #M.2.sink.eq7.k)

  # scalar out
  B.mk <- with(param.set,
                 eq07_Bmk(W2,
                          K.bc,
                          B.fk))


  ### EQUATION 8:  eq08calcScalar()
  ### Males that don't find a territory become "drain" males (floaters)

  #   W2["mg"]+W2["mp"] = total male population
  #   minus those that ended up in source habitat (K.bc)
  #   minus those that paired w/female in sink habitat (B.fk)

  B.md <- with(param.set,
                 eq08_Bmd(W2,
                          K.bc,
                          B.fk)) #M.2.drain.eq8



  ###############################
  ### Summer Pairing dynamics ###
  ###############################

  # Equations 9 through 16
  # all output are scalars

  ### EQUATION 9 eq09calcScalar()

  #### alias #pairing.eq9.P.c.gg

  P.cgg <- eq09_Pcgg(W2,
                     K.bc = param.set$K.bc,
                     B.mc = B.mc,
                     B.fc = B.fc)

  ### EQUATION 10: eq10()
  ### proportion of pairs on source (c) habitat composed of males from good and
  ### female from poor

  # alias #pairing.eq10.P.c.gp

  P.cgp <- eq10_Pcgp(W2,
                     K.bc = param.set$K.bc,
                       B.fc = B.fc,
                       B.mc = B.mc)



  ### EQUATION 11:  eq11()
  ### Proportion of poor males mated w/ "good" female
  #### NOte: subscripts wrong in original paper

  P.cpg <- eq11_Pcpg(W2 = W2,
                          K.bc = param.set$K.bc,
                          B.fc = B.fc,
                          B.mc = B.mc)




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
                       B.mk = B.mk,
                       B.fk = B.fk)



    ### EQUATION 14: eq14()
    ### proportion in sink habitat, good-poor pairs

    #### alias pairing.eq14.P.kgp
    P.kgp <- eq14_Pkgp(W2 = W2,      #note that both eq14 and eq habve .kgp subscripts in original paper
                            K.bc = param.set$K.bc,
                            K.bk = param.set$K.bk,
                            B.mk = B.mk,
                            B.fk = B.fk)


    ### EQUATION 15: eq15()
    ### aliaspairing.eq15.P.kpg
    ### note that both eq14 and eq habve .kgp subscripts in original paper
    P.kpg <- eq15_Pkpg(W2,
                       param.set$K.bc,
                       param.set$K.bk,
                       B.mk, B.fk)



    ### EQUATION 16: eq16()
    #   proportion in sink composed of poor-poor

    #### alias pairing.eq16.P.kpp
    P.kpp <- eq16_Pkpp(P.kgg, P.kgp, P.kpg,
                       B.mk, B.fk)




    ### EQUATION 17: eq17buildVect()
    ### vector of results after prior to pairing

    B0 <- eq17buildVect(B.mc,
                        B.mk,
                        B.md,
                        B.fc,
                        B.fk)

   ### Check B0 for errors
   ###
   error_check_B0(B0 = B0,
                  W2 = W2,
                  check.errors.in = check.errors.in,
                  i = i)

   ### Check P.xxx for errors
   ###  Loops over

   #set up
   P.all.vector <- c(P.cgg, P.cgp,P.cpg, P.cpp,P.kgg, P.kgp,P.kpg, P.kpp)
   names(P.all.vector) <- c("P.cgg", "P.cgp","P.cpg", "P.cpp","P.kgg", "P.kgp","P.kpg", "P.kpp")

   P.equation.names    <- c("eq9_Pcgg", "eq10_Pcgp","eq11_Pcpg","eq12_Pcpp",
                            "eq13_Pkgg","eq14_Pkgp","eq15_Pkpg","eq16_Pkpp")

   names(P.equation.names) <- c("P.cgg", "P.cgp","P.cpg", "P.cpp","P.kgg", "P.kgp","P.kpg", "P.kpp")

   #subset work
   P.xxx.names <- check.errors.in[grep("P.",check.errors.in)]
   P.xxx.values <- P.all.vector[P.xxx.names]
   P.xxx.eq.names <- P.equation.names[P.xxx.names]
   if(length(P.xxx.names) > 0){
     for(p in 1:length(P.xxx.names)){
       error_check_Pxxx(P.xxx.name = P.xxx.names[p],
                        P.xxx.value = P.xxx.values[p],
                        P.xxx.eq.name = P.xxx.eq.names[p],
                        i = i)}
   }





    ###########################
    ### Summer Reproduction ###
    ###########################


    ### REPRODUCTION
    #"average fecunidty for pairs in source and sink habitat"
    # is a function of how the proportion of pairs in that
    # habitat that are good-good, good-poor etc


    ### LOAD EQUATION 18a
    ### alias: fx.make.R.all.eq18
    R.all <- with(param.set,
                  eq18buildRmat(R.base.rate,
                                R.hab.effect,
                                co))


    #Delete this?: ## EQUATION 18
    #Delete this?: LOAD QUATION 18b
    #Delete this?:  all.Ps <- c(P.cgg, P.cgp, P.cpg, P.cpp,
    #Delete this?:              P.kgg, P.kgp, P.kpg, P.kpp)

    #Fx.make.P.matrix.eq18

    # if(round(sum(P.cgg, P.cgp,P.cpg, P.cpp),3) != 1)
    # if(round(sum(P.kgg, P.kgp,P.kpg, P.kpp),3) > 1)
    # if(round(sum(P.kgg, P.kgp,P.kpg, P.kpp),3) < 0)




    P.all <- eq18buildPmat(P.cgg, P.cgp,
                           P.cpg, P.cpp,
                           P.kgg, P.kgp,
                           P.kpg, P.kpp)

    #APPLY EQUATION 18
    R <- P.all%*%R.all


    ### Equation 19: eq19buildMinMat() ###
    #### Alias Fx.make.min.mat.eq19
    eq19.min.mat <- eq19buildMinMat(B.mc,
                                    B.fc,
                                    B.mk,
                                    B.fk)

    #sex ratio

    #make.F.matrix
    eq19.f.mat <- with(param.set,
                       eq19buildFmat(f))


    ### Cacluate reproductive output
    Y1 <- eq19.f.mat%*%eq19.min.mat%*%R
    names(Y1) <- c("mc","mk","fc","fk")



    ### Cacluate reproductive output
    if(any(is.na(Y1)) == TRUE |
       any(is.nan(Y1)) == TRUE){
      browser()
    }




    ##############################
    ### Summer adult mortality ###
    ##############################


    ### BREEDING season mortality
    #"adult birds experienc both sex- and habitat specific mortality
    # over the breeding season.

    ### Equation 20:  ###
    #### eq20_build_Sb_mat() builds the S.b matrix
    B1 <- S.b%*%B0


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


    #"Mort during mig depends upon the sex of the bird &
    # breeding habitat it used"

    ### EQUATION 21:   ###
    ### eq21_build_Sf_mat() builds S.f
    B2 <- S.f%*%B1

    names(B2) <- c("mc","mk","md","fc","fk")




    #------------------------------------#
    #   MIGRATION Mortality of young (y) #
    #------------------------------------#


    ### EQUATION 22 eq22() ###
    Y2 <- S.y%*%Y1; names(Y2) <- c("y.mc","y.mk","y.fc","y.fk")




    #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
    #*#*#                             #*#*#
    #*#*# Winter competitiom Dynamics #*#*#
    #*#*#                             #*#*#
    #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

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




    # if(i == 65){
    #   #
    #   #debugonce(eq24_comp_loop)
    #   #debugonce(eq26_alloc_winter_P)
    # }

    ## Run eq24_comp_loop()
    ### Outputs a list

    comp.out.list <- eq24_comp_loop(A.i.0 = A.i.0,
                                    K.wg.0 = K.wg.0,
                                    y.i = gamma.i,
                                    comp.df = df,
                                    j = comp.its, i = i)




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



    ### save current state
    ###  pick up everything from environment & save
    #### NOTE: this could probably be made more efficient
    ####       right now it picks up entire df frame, adds data to current
    ####       column i, then outputs entire df
    #### Also, to speed thins up I should
    #### make it optional as to whether this is saved - what is
    #### really only needed for most runs is final equilibrium sizes

    #

    if(save.ts == TRUE){
      out.df <- save_FAC_state(i, out.df,
                               W.list$W.mg,W.list$W.mp,W.list$W.fg,W.list$W.fp,
                                   B0,
                                   P.cgg, P.cgp, P.cpg, P.cpp,
                                   P.kgg, P.kgp, P.kpg, P.kpp,
                                   Y2,
                                   A.i.G,
                                   A.i.P)

      ### Check to see if equilibrium has been reached
      ###  population size no longer changing
      at.eq <- FALSE
      if(check.eq == TRUE & iterations > check.eq.after.i & i > iterations/tol.1){
        at.eq <- runFAC_check_equilibrium(out.df, i, tol.2,at.eq)
      }

      if(at.eq == TRUE){break}
    }


    ## If not saving full time series then save final state at last time point
    if(save.ts == FALSE & i == iterations){

      out.eq <- save_FAC_state(i, out.df,
                                   W.list$W.mg,W.list$W.mp,W.list$W.fg,W.list$W.fp,
                                   B0,
                                   P.cgg, P.cgp, P.cpg, P.cpp,
                                   P.kgg, P.kgp, P.kpg, P.kpp,
                                   Y2,
                                   A.i.G,
                                   A.i.P)[iterations,]
    }




  }#close main for() loop for iterating model



  ### Finalize output dataframe
  ### Total up seasonal population sizes, round off numbers
  if(save.ts == TRUE){
    out.df <- runFAC_finalize_output(out.df)

    ### Plot Diagnostic for full run of model
    if(diagnostic.plot == T){
      plot_runFAC(out.df)
    }


    ### Return output of full run of model
    if(return.output == T){


      return(out.df)
    }
  }


### If NOT saving full time series just output eq. state at the end
if(save.ts == FALSE){
  ## would be good if I could calculate lambda to
  ## monitor equilibrium
  return(out.eq)
}








}
