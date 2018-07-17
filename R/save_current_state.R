
#' Save current population state
#'
#'
#' @param i current iteration of model
#' @param out.df dataframe holding output of model for each iteraction
#' @param W.mg scalar with number of males at beginning of winter in good habitat
#' @param W.mp scalar with number of males at beginning of winter in poor habitat
#' @param W.fg scalar with number of females at beginning of winter in good habitat
#' @param W.fp scalar with number of females at beginning of winter in poor habitat
#' @param B0 vector of...
#' @param P.cgg scalar representing pairing frequency of birds in source (".c") habitat the both originated from good (".gg") habitat on the wintering ground.
#' @param P.cgp scalar...
#' @param P.cpp scalar...
#' @param P.kgg scalar...
#' @param P.kgp scalar...
#' @param P.kpg scalar...
#' @param P.kpp scalar...
#' @param Y2 vector of reproductive output (post migration?)
#' @param A.G vector...
#' @param A.P vector...
#'
#' @return out.df dataframe updated with population sizeds from time i
#'
#' @export
#'


save_current_state <- function(i, out.df,
                             W.mg,W.mp,W.fg,W.fp,
                             B0,
                             P.cgg, P.cgp, P.cpg, P.cpp,
                             P.kgg, P.kgp, P.kpg, P.kpp,
                             Y2,
                             A.G,
                             A.P, ...){

  ### SAVE POPULATION STATE
  #### Save winter state "after ...birds have arrived and settelined onto territories"
  out.df[i,c("W.mg","W.mp","W.fg","W.fp")] <- c(W.mg,W.mp,W.fg,W.fp)


  #### SAVE SUMMER state ###
  out.df[i,c("B.mc","B.mk","B.md","B.fc","B.fk")] <- B0

  ### SAVE pairing frequencies
  #equation 18ish

  out.df[i, c("P.cgg","P.cgp","P.cpg","P.cpp",
              "P.kgg","P.kgp","P.kpg","P.kpp")] <- c(P.cgg, P.cgp, P.cpg, P.cpp,
                                                     P.kgg, P.kgp, P.kpg, P.kpp)



  ### Save offspring
  out.df[i, c("y.mc","y.mk","y.fc","y.fk")] <- Y2


  # Store number of birds allocated to G and P habitat
  out.df[i,grep("A.G",names(out.df))] <- A.G #
  out.df[i,grep("A.P",names(out.df))] <- A.P

  return(out.df)
}


