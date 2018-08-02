#' Make empty dataframe for holding output of runMultiFAC(). which runs the model accross a range of paramers
#'
#' @export

make_multiFAC_df <- function(params.use = param_grid()){

  FAC.runs <- nrow(params.use)
  NA.vec <- rep(NA,FAC.runs)

  #dataframe only holds key information about equilibrium
  MultiFAC.out.df <- data.frame(B.mc = NA.vec,
                                B.mk = NA,
                                B.md = NA,
                                B.fc = NA,
                                B.fk = NA,
                                W.mg = NA,
                                W.mp = NA,
                                W.fg = NA,
                                W.fp = NA,
                                error1 = NA,
                                error2 = NA)
  return(MultiFAC.out.df)
}
