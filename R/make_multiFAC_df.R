#' Make empty dataframe for holding output of runMultiFAC()
#'
#' @param params.use Dataframe of parameter values being used in the model.
#'
#' @export

make_multiFAC_df <- function(params.use = param_grid()){

  # size of parameter grid
  FAC.runs <- nrow(params.use)

  #empty vector for building df
  NA.vec <- rep(NA,FAC.runs)

  #dataframe only holds key information about equilibrium
  MultiFAC.out.df <- data.frame(B.mc = NA.vec, #
                                B.mk = NA,
                                B.md = NA,
                                B.fc = NA,
                                B.fk = NA,
                                W.mg = NA,
                                W.mp = NA,
                                W.fg = NA,
                                W.fp = NA,

                                P.cgg= NA,  #pairing frequencies
                                P.cgp= NA,
                                P.cpg= NA,
                                P.cpp= NA,
                                P.kgg= NA,
                                P.kgp= NA,
                                P.kpg= NA,
                                P.kpp= NA,

                                y.mc= NA, #offspring by origin
                                y.mk= NA,
                                y.fc= NA,
                                y.fk= NA,

                                A.G.mc= NA, #competition
                                A.G.mk= NA,
                                A.G.md= NA,
                                A.G.y.mc= NA,
                                A.G.y.mk= NA,
                                A.G.fc= NA,
                                A.G.fk= NA,
                                A.G.y.fc= NA,
                                A.G.y.fk= NA,

                                A.P.mc= NA,
                                A.P.mk= NA,
                                A.P.md= NA,
                                A.P.y.mc= NA,
                                A.P.y.mk= NA,
                                A.P.fc= NA,
                                A.P.fk= NA,
                                A.P.y.fc= NA,
                                A.P.y.fk= NA,

                                error1 = NA, #error codes
                                error2 = NA)
  return(MultiFAC.out.df)
}
