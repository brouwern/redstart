#' Run multiple FAC models to equilibirum accross a range of parameters
#'
#' Note that the "para.grid" arguement take take produced by the param_grid() function,
#' which gets its name because it uses the expand.grid() function to make all
#' combinations of the parameters being varied.
#' runFAC() has been updated to be able to output the full time serie
#' OR just the eq pop size
#' need to update runFAC_multi() to just take that eq size
#' should run a bit faster if runFAC() is keep all of that info
#' NB: "remakeFigure" does not appear in current code
#'
#' @param para.grid Dataframe of parameters, usually with 1 or 2 that vary accross a range
#' @param Ninit vector of initial population sizes
#' @param makeFigure deprecated.  Replaced by plot_Fig29_3
#' @param use.IBM Use individual-based model (IBM) implementation
#' @param eq.tol Tolerance level for testing for equilibirum.  Smaller values are more stringent.
#' @param ... other arguements
#'
#' @export


runFAC_multi <- function(param.grid = param_grid(),
                         Ninit = c(10,0,10,0),
                         remakeFigure = NA,
                         use.IBM = F,
                         verbose = F,
                         eq.tol = 6,
                         ...){


  ### DATA STORAGE FOR runMultiFAC
  ### df to store output of each individual model run
  multiFAC.out.df.RM <- make_multiFAC_df(params.use=param.grid)
  multiFAC.out.df.IB <- multiFAC.out.df.RM



  ### Iterate models over each set of parametres
  for(i in 1:dim(param.grid)[1]){

    runFAC.i <-  runFAC(param.set=param.grid[i,],
                        Ninit = Ninit,
                      check.eq = TRUE,
                      save.ts = FALSE,
                      diagnostic.plot = F,
                      verbose = verbose,
                      eq.tol = eq.tol,
                      use.IBM = use.IBM)




    ### Extract and store final output of each iteration of model, the equilibrium population size
    FAC.eq.state.RM <- runFAC.i$FAC.eq.state.RM

    if(use.IBM == TRUE){
      FAC.eq.state.IB <- runFAC.i$FAC.eq.state.IB
    }

    focal.out <- c("B.mc","B.mk","B.md",
                   "B.fc","B.fk",
                   "W.mg","W.mp",
                   "W.fg","W.fp")



    FAC.eq.state.focal.output.RM <- FAC.eq.state.RM[focal.out]
    multiFAC.out.df.RM[i,focal.out] <- FAC.eq.state.focal.output.RM


    if(use.IBM == TRUE){
      FAC.eq.state.focal.output.IB <- FAC.eq.state.IB[focal.out]
      multiFAC.out.df.IB[i,focal.out] <- FAC.eq.state.focal.output.IB
    }


    #store parameters used to run the model
    multiFAC.out.df.RM$gamma.i[i] <- param.grid$gamma[i]
    multiFAC.out.df.RM$c.i[i]     <- param.grid$c[i]
    multiFAC.out.df.RM$K.bc.i[i]  <- param.grid$K.bc[i]
    multiFAC.out.df.RM$K.wg.i[i]  <- param.grid$K.wg[i]

    if(use.IBM == TRUE){
      multiFAC.out.df.IB$gamma.i[i] <- param.grid$gamma[i]
      multiFAC.out.df.IB$c.i[i]     <- param.grid$c[i]
      multiFAC.out.df.IB$K.bc.i[i]  <- param.grid$K.bc[i]
      multiFAC.out.df.IB$K.wg.i[i]  <- param.grid$K.wg[i]

    }


    #look for errors

    multiFAC.out.df.RM[i,"error1"] <- ifelse(any(multiFAC.out.df.RM < 0), "neg values",".")
    #multiFAC.out.df.RM[i,"error2"] <- ifelse(any(multiFAC.out.df.RM[,P.columns] > 1), "p>1",".")


    if(use.IBM == TRUE){

      multiFAC.out.df.IB[i,"error1"] <- ifelse(any(multiFAC.out.df.IB < 0), "neg values",".")
      #multiFAC.out.df.IB[i,"error2"] <- ifelse(any(multiFAC.out.df.IB[,P.columns] > 1), "p>1",".")

    }

  }#close for loop to iterate model


  #### Process model output
  ### Calculate totals by sex and sex ratio
  multiFAC.out.df.RM <- runFAC_multi_finalize_output(multiFAC.out.df.RM,
                                                     param.grid)

  if(use.IBM == TRUE){
    multiFAC.out.df.IB <- runFAC_multi_finalize_output(multiFAC.out.df.IB,
                                                       param.grid)


  }


  multiFAC.out <- list(multiFAC.out.df.RM = multiFAC.out.df.RM,
                       multiFAC.out.df.IB = multiFAC.out.df.IB)

  return(multiFAC.out)
}#closefunction
