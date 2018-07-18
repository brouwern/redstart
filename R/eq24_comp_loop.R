
eq24_comp_loop <- function(A.i.0,
                           K.wg.0 ,
                           y.i,
                           comp.df,
                           j,
                           ...
                           ){

  #number of iterations to loop
  index.j <- 1:j

  # Assign intial states
  ## Initial number of settled birds
  A.G.i.settled <- rep(0, length(A.i.0))

  ## Number of open territories = carrying capacity
  K.wg.j <- K.wg.0

  ## Number of actively competiing birds
  A.i.active.j <- A.i.0




  #browser()
  #competition loop
  for (j in index.j){

    ## If total pop size is less than carrying capacity
    ## all birds settle in good habitat
    ## and loop exists
    if(sum(A.i.active.j) <= K.wg.0){
      A.G.i.settled <- A.i.0

      break
    }

    #store for reference
    K.wg.j.init <- K.wg.j
    A.G.i.settled.init <- sum(A.G.i.settled)
    A.G.i.active.init  <- sum(A.i.active.j)

    ## settled best competitors to good habitat
    #A.i.settled.j.raw <- (A.i.active.j*y.i)/sum((A.i.active.j*y.i))*K.wg.j
    A.i.settled.j.raw <- eq24_competition()

    ## correct values if they exceed original abundance A.i.0
    #
    A.i.settled.j.cor <- eq25_comp_constrain()

    ## Add those just settled to those already in good habitat
    A.G.i.settled <- A.i.settled.j.cor + A.G.i.settled

    ## Update carrying capacity
    K.wg.j <- K.wg.0 - sum(A.G.i.settled)

    ## calculate number remaining un-settledd
    A.i.unsettled.j <- A.i.active.j - A.i.settled.j.cor


    ## Bird unsettled at end of loop remain "Active" during next iteration
    A.i.active.j <- A.i.unsettled.j

    ### Save meta data
    comp.df$j[j] <- j
    comp.df$K.wg.j.init[j]         <- K.wg.j.init
    comp.df$K.wg.j.end[j]         <- K.wg.j
    comp.df$tot.settled.init[j]    <- A.G.i.settled.init
    comp.df$tot.settled.final[j]   <- sum(A.G.i.settled)
    comp.df$tot.active.init[j]     <- A.G.i.active.init
    comp.df$tot.active.final[j]    <- sum(A.i.active.j)
    comp.df$suc.settled.raw[j]    <- sum(A.i.settled.j.raw)
    comp.df$suc.settled.cor[j]    <- sum(A.i.settled.j.cor)
    comp.df$un.settled[j]         <- sum(A.i.unsettled.j)



    if(sum(A.G.i.settled) == K.wg){ break }

  }

  comp.df <- stats::na.omit(comp.df)

  comp.df <- apply(comp.df,2,round)

  comp.list <- list(A.G.i.settled = A.G.i.settled,
                    comp.df = comp.df)

  return(comp.list)
}

