#' Finalize output from runFAC_multi
#'
#' @param multiFAC.out.df Output of runFAC_multi
#' @param param.grid Grid of parameters over which model was run (?)
#'
#' @export
#'


runFAC_multi_finalize_output <- function(multiFAC.out.df,
                                         param.grid){

  multiFAC.out.df$B.m.tot        <- apply(multiFAC.out.df[,c("B.mc","B.mk","B.md")],1,sum)
  multiFAC.out.df$B.m.tot.no.d   <- apply(multiFAC.out.df[,c("B.mc","B.mk")],1,sum)
  multiFAC.out.df$B.f.tot        <- apply(multiFAC.out.df[,c("B.fc","B.fk")],1,sum)
  multiFAC.out.df$sex.ratio      <- multiFAC.out.df$B.m.tot/multiFAC.out.df$B.f.tot
  multiFAC.out.df$sex.ratio.no.d <- multiFAC.out.df$B.m.tot.no.d/multiFAC.out.df$B.f.tot


  ### Calculate total breeding population size
  multiFAC.out.df$B.tot <- multiFAC.out.df$B.m.tot +  multiFAC.out.df$B.f.tot
  multiFAC.out.df$B.tot.no.d <- multiFAC.out.df$B.m.tot.no.d +  multiFAC.out.df$B.f.tot


  multiFAC.out.df$variable <- 1:dim(param.grid)[1]

  return(multiFAC.out.df)
}

