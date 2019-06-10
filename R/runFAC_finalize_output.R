#' Finalize output of a single run of FAC
#'
#' @param out.df Dataframe of output
#' @param save.ts Should the time series of population sizes be saved?
#'
#' @return out.df Dataframe ...
#'
#' @export

runFAC_finalize_output <- function(out.df,
                                   save.ts = TRUE){
  ### Summarize model run



  # time
  out.df$t     <- 1:dim(out.df)[1]

  # total breeding population

  out.df$tot.B <- apply(out.df[,c("B.mc","B.mk",
                                  "B.md",
                                  "B.fc","B.fk")],1,sum)


  # total winter population
  out.df$tot.W <- apply(out.df[,c("W.mg","W.mp",
                                  "W.fg","W.fp")],1,sum)

  # ???
  out.df$tot.P.c <- apply(out.df[,c("P.cgg","P.cgp","P.cpg","P.cpp")],1,sum)

  # ???
  out.df$tot.P.k <- apply(out.df[,c("P.kgg","P.kgp","P.kpg","P.kpp")],1,sum)




  # round result

  #out.df[,apply(out.df,2, is.numeric)] <- round(out.df[,apply(out.df,2, is.numeric)],2)

  # remove any rows not used
  #out.df <- na.omit(out.df)


  ### Summarize population growth rate (lamda)
  #### can only be done if full time series saved
  if(save.ts == TRUE){
    t.max <- nrow(out.df)
    t.max.min1 <- nrow(out.df) -1

    out.df$lambda <- c(NA,out.df$tot.W[2:t.max]/out.df$tot.W[1:t.max.min1])


  }


  return(out.df)
}

