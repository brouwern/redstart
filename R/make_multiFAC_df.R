#Make empty dataframe for holding output of runMultiFAC(). which runs the model accross a range
#of paramers
make_multiFAC_df <- function(params.use){

  FAC.runs <- rnow(params.use)
  NA.vec <- rep(NA,FAC.runs)

  #dataframe only holds key information about equilibrium
  MultiFAC.out.df <- data.frame(B.mc = NA.vec,
                                B.mk = NA.vec,
                                B.md = NA.vec,
                                B.fc = NA.vec,
                                B.fk = NA.vec,
                                W.mg = NA.vec,
                                W.mp = NA.vec,
                                W.fg = NA.vec,
                                W.fp = NA.vec,
                                error1 = NA.vec,
                                error2 = NA.vec,
                                err1.log = NA.vec,
                                err2.log = NA.vec)
  return(MultiFAC.out.df)
}
