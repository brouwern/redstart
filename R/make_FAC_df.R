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
                         W.mp = NA.vec,  #m = male, f = female
                         W.fg = NA.vec,  #g = good, p = poor
                         W.fp = NA.vec,

                         B.mc = NA.vec,  #Breeding population
                         B.mk = NA.vec,  # c = source, d = drain, k = sink
                         B.md = NA.vec,
                         B.fc = NA.vec,
                         B.fk = NA.vec,

                         P.cgg = NA.vec, #Paring frequencies between males-femals from different types of winter habitat
                         P.cgp = NA.vec, # gg = pairing between male from good habitat and female from good habitat
                         P.cpg = NA.vec,
                         P.cpp = NA.vec,
                         P.kgg = NA.vec,
                         P.kgp = NA.vec,
                         P.kpg = NA.vec,
                         P.kpp = NA.vec,

                         y.mc = NA.vec,   #Offspring
                         y.mk = NA.vec,
                         y.fc = NA.vec,
                         y.fk = NA.vec,

                         A.G.mc = NA.vec,  #Winter population BEFORE competition
                         A.G.mk = NA.vec,  #G = good habitat, P = poor
                         A.G.md = NA.vec,  #mc = males from summer source
                         A.G.y.mc = NA.vec,#mk = males from summer sink
                         A.G.y.mk = NA.vec,#md = males from summer drain
                         #y = young, eg y.mc = young males born in source habitat
                         A.G.fc = NA.vec,
                         A.G.fk = NA.vec,
                         A.G.y.fc = NA.vec,
                         A.G.y.fk = NA.vec,#

                         A.P.mc = NA.vec,
                         A.P.mk = NA.vec,
                         A.P.md = NA.vec,
                         A.P.y.mc = NA.vec,
                         A.P.y.mk = NA.vec,
                         A.P.fc = NA.vec,
                         A.P.fk = NA.vec,
                         A.P.y.fc = NA.vec,
                         A.P.y.fk = NA.vec)
  return(out.df)
}
