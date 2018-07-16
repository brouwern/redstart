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
#' @param makeFigure deprecated.  Replaced by plot_Fig29_3
#' @param ... other arguements
#'
#' @export


runFAC_multi <- function(param.grid = param_grid(),
                         remakeFigure = NA,
                         ...){


  ### DATA STORAGE FOR runMultiFAC
  ### df to store output of each individual model run
  multiFAC.out.df <- make_multiFAC_df(params.use=param.grid)



  ### Iterate models over each set of parametres
  for(i in 1:dim(param.grid)[1]){

    out.df <-  runFAC(param.set=param.grid[i,],
                      check.eq = FALSE,
                      diagnostic.plot = F)




    ### Extract and store final output of each iteration of model, the equilibrium population size
    i.lastrow <- dim(out.df)[1]

    multiFAC.out.df[i,c("B.mc","B.mk","B.md",
                        "B.fc","B.fk",
                        "W.mg","W.mp",
                        "W.fg","W.fp")] <- out.df[i.lastrow,c(
                          "B.mc","B.mk","B.md",
                          "B.fc","B.fk",
                          "W.mg","W.mp",
                          "W.fg","W.fp")]


    #store parameters used to run the model
    multiFAC.out.df$gamma.i[i] <- param.grid$gamma[i]
    multiFAC.out.df$c.i[i] <- param.grid$c[i]
    multiFAC.out.df$K.bc.i[i] <- param.grid$K.bc[i]
    multiFAC.out.df$K.wg.i[i] <- param.grid$K.wg[i]

    #look for errors
    P.columns <- c("P.cgg","P.cgp","P.cpg","P.cpp",
                   "P.kgg","P.kgp","P.kpg","P.kpp")

    multiFAC.out.df[i,"error1"] <- ifelse(any(out.df < 0), "neg values",".")
    multiFAC.out.df[i,"error2"] <- ifelse(any(out.df[,P.columns] > 1), "p>1",".")

  }#close for loop to iterate model



  #### Process model output
  ### Calculate totals by sex and sex ratio
  multiFAC.out.df$B.m.tot <- apply(multiFAC.out.df[,c("B.mc","B.mk","B.md")],1,sum)
  multiFAC.out.df$B.m.tot.no.d <- apply(multiFAC.out.df[,c("B.mc","B.mk")],1,sum)
  multiFAC.out.df$B.f.tot <- apply(multiFAC.out.df[,c("B.fc","B.fk")],1,sum)
  multiFAC.out.df$sex.ratio <- multiFAC.out.df$B.m.tot/multiFAC.out.df$B.f.tot
  multiFAC.out.df$sex.ratio.no.d <- multiFAC.out.df$B.m.tot.no.d/multiFAC.out.df$B.f.tot


  ### Calculate total breeding population size
  multiFAC.out.df$B.tot <- multiFAC.out.df$B.m.tot +  multiFAC.out.df$B.f.tot
  multiFAC.out.df$B.tot.no.d <- multiFAC.out.df$B.m.tot.no.d +  multiFAC.out.df$B.f.tot


  multiFAC.out.df$variable <- 1:dim(param.grid)[1]

  return(multiFAC.out.df)
}#closefunction
