#' Create FAC output dataframe
#'
#' Creates a dataframe to hold output from a single run
#' of a Runge & Marra (2004) FAC model Model is run over a certain number of
#' iterations (reps) inorder to reach equilibrium. The main output of a full run
#' of model is the equilibrium population size.
#'
#' Formerly aliased as fx.create.out.df
#'
#' @param reps Number of reps to run basic FAC model. Defaults to 300.
#'
#' @return out.df A dataframe to hold output of each iteration from a single run of the model.
#'
#' @export


make_FAC_df <- function(iterations = 300){ #change reps to iterations?

  # Empty vector
  NA.vec <- rep(NA,iterations)

  # Dataframe to hold output
  out.df <- data.frame(W.mg = NA.vec,  #Winter population AFTER Competition
                         W.mp = NA,  #m = male, f = female
                         W.fg = NA,  #g = good, p = poor
                         W.fp = NA,

                         B.mc = NA,  #Breeding population
                         B.mk = NA,  # c = source, d = drain, k = sink
                         B.md = NA,
                         B.fc = NA,
                         B.fk = NA,

                         P.cgg = NA, #Paring freqs btwn m-f from diff types of winter habitat
                         P.cgp = NA, # gg = pairing btwn m from good habitat & f from good habitat
                         P.cpg = NA,
                         P.cpp = NA,
                         P.kgg = NA,
                         P.kgp = NA,
                         P.kpg = NA,
                         P.kpp = NA,

                         y.mc = NA,   #Offspring
                         y.mk = NA,
                         y.fc = NA,
                         y.fk = NA,

                         A.G.mc = NA,  #Winter population BEFORE competition
                         A.G.mk = NA,  #G = good habitat, P = poor
                         A.G.md = NA,  #mc = males from summer source
                         A.G.y.mc = NA,#mk = males from summer sink
                         A.G.y.mk = NA,#md = males from summer drain
                         #y = young, eg y.mc = young males born in source habitat
                         A.G.fc = NA,
                         A.G.fk = NA,
                         A.G.y.fc = NA,
                         A.G.y.fk = NA,#

                         A.P.mc = NA,
                         A.P.mk = NA,
                         A.P.md = NA,
                         A.P.y.mc = NA,
                         A.P.y.mk = NA,
                         A.P.fc = NA,
                         A.P.fk = NA,
                         A.P.y.fc = NA,
                         A.P.y.fk = NA,
                         lambda.B.mc.i = NA,
                       lamb.B.mc.mean = NA,
                       lamb.B.mc.var = NA) #calculate of pop growth over 1 time step for diagnostic
  return(out.df)
}
