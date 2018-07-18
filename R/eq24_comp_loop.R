#' Competition loop
#'
#' @export


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
  ## When loop starts, no birds have settled on territories
  A.i.G.settled.j <- rep(0, length(A.i.0))

  ## Number of open territories
  ###  When loop starts all are open
  K.wg.open.j <- K.wg.0

  ## Number of actively competiing birds
  ###  (initilally = pop vector)
  A.i.active.j <- A.i.0





  #competition loop
  for (j in index.j){

    ## If total pop size is less than carrying capacity
    ## all birds settle in good habitat
    ## and loop exists
    if(sum(A.i.active.j) <= K.wg.0){
      A.i.G.settled.j <- A.i.0

      break
    }



          #store for reference
          K.wg.open.j.init <- K.wg.open.j
          A.i.G.settled.init <- sum(A.i.G.settled.j)
          A.G.i.active.init  <- sum(A.i.active.j)

    ## settled best competitors to good habitat
    #A.i.settled.raw.j <- (A.i.active.j*y.i)/sum((A.i.active.j*y.i))*K.wg.open.j
    A.i.G.settled.raw.j <- eq24_competition(A.i.j = A.i.active.j,
                                          y.i = y.i,
                                          K.wg.open.j = K.wg.open.j)

    ## correct values if they exceed original abundance A.i.0
    #
    A.i.G.settled.cor.j <- eq25_comp_constrain(A.i.G.settled.raw.j = A.i.G.settled.raw.j,
                                             A.i.0 = A.i.0)

    ## Add those just settled to those already in good habitat
    #total settled    total settled         total settled
    #overwritten      this iteration        current
    A.i.G.settled.j <- A.i.G.settled.cor.j + A.i.G.settled.j

    ## Update carrying capacity
    K.wg.open.j <- K.wg.0 - sum(A.i.G.settled.j)

    ## calculate number remaining un-settledd
    # currently      original  updated number
    # un-settled     pop vect  settled
    A.i.unsettled.j <- A.i.0 - A.i.G.settled.j


    ## Bird unsettled at end of loop remain "Active" during next iteration
    A.i.active.j <- A.i.unsettled.j

    ### Save meta data
    comp.df$j[j] <- j
    comp.df$K.wg.open.j.init[j]         <- K.wg.open.j.init
    comp.df$K.wg.open.j.end[j]         <- K.wg.open.j
    comp.df$tot.settled.init[j]    <- A.i.G.settled.init
    comp.df$tot.settled.final[j]   <- sum(A.i.G.settled.j)
    comp.df$tot.active.init[j]     <- A.G.i.active.init
    comp.df$tot.active.final[j]    <- sum(A.i.active.j)
    comp.df$suc.settled.raw[j]    <- sum(A.i.G.settled.raw.j)
    comp.df$suc.settled.cor[j]    <- sum(A.i.G.settled.cor.j)
    comp.df$un.settled[j]         <- sum(A.i.unsettled.j)


    #If number still active = 0, stop
    if(sum(A.i.active.j) == 0){ break }

    #If number settled = carrying capacity, stop
    if(round(sum(A.i.G.settled.j),0) >= K.wg.0){ break }
  }

  comp.df <- stats::na.omit(comp.df)

  comp.df <- apply(comp.df,2,round)

  comp.list <- list(A.i.G.settled.j = A.i.G.settled.j,
                    comp.df = comp.df)

  return(comp.list)
}

