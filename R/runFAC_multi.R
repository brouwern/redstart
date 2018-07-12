
#"remakeFigure" does not appear in current code
#should I set up runFAC() to be able to only output equilibrium size,
# and have that be the only thing that gets picked up by runFAC_multi()
# should run a bit fast if runFAC() is keep all of that info

runFAC_multi <- function(param.grid = param_grid(),
                         remakeFigure = NA){


  ### DATA STORAGE FOR runMultiFAC
  ### df to store output of each individual model run
  MultiFAC.out.df <- make_MultiFAC_out_df(params.use=param.grid)



  ### Iterate models over each set of parametres
  for(i in 1:dim(param.grid)[1]){

    out.df <-  runFAC(paramsWorking=param.grid[i,])




    ### Extract and store final output of each iteration of model, the equilibrium population size
    i.lastrow <- dim(out.df)[1]

    MultiFAC.out.df[i,c("B.mc","B.mk","B.md",
                        "B.fc","B.fk",
                        "W.mg","W.mp",
                        "W.fg","W.fp")] <- out.df[i.lastrow,c(
                          "B.mc","B.mk","B.md",
                          "B.fc","B.fk",
                          "W.mg","W.mp",
                          "W.fg","W.fp")]


    #store parameters used to run the model
    MultiFAC.out.df$gamma.i[i] <- param.grid$gamma[i]
    MultiFAC.out.df$c.i[i] <- param.grid$c[i]
    MultiFAC.out.df$K.bc.i[i] <- param.grid$K.bc[i]
    MultiFAC.out.df$K.wg.i[i] <- param.grid$K.wg[i]

    #look for errors
    P.columns <- c("P.cgg","P.cgp","P.cpg","P.cpp",
                   "P.kgg","P.kgp","P.kpg","P.kpp")

    MultiFAC.out.df[i,"error1"] <- ifelse(any(out.df < 0), "neg values",".")
    MultiFAC.out.df[i,"error2"] <- ifelse(any(out.df[,P.columns] > 1), "p>1",".")

  }#close for loop to iterate model



  #### Process model output
  ### Calculate totals by sex and sex ratio
  MultiFAC.out.df$B.m.tot <- apply(MultiFAC.out.df[,c("B.mc","B.mk","B.md")],1,sum)
  MultiFAC.out.df$B.m.tot.no.d <- apply(MultiFAC.out.df[,c("B.mc","B.mk")],1,sum)
  MultiFAC.out.df$B.f.tot <- apply(MultiFAC.out.df[,c("B.fc","B.fk")],1,sum)
  MultiFAC.out.df$sex.ratio <- MultiFAC.out.df$B.m.tot/MultiFAC.out.df$B.f.tot
  MultiFAC.out.df$sex.ratio.no.d <- MultiFAC.out.df$B.m.tot.no.d/MultiFAC.out.df$B.f.tot


  ### Calculate total breeding population size
  MultiFAC.out.df$B.tot <- MultiFAC.out.df$B.m.tot +  MultiFAC.out.df$B.f.tot
  MultiFAC.out.df$B.tot.no.d <- MultiFAC.out.df$B.m.tot.no.d +  MultiFAC.out.df$B.f.tot


  MultiFAC.out.df$variable <- 1:dim(param.grid)[1]

  return(MultiFAC.out.df)
}#closefunction
