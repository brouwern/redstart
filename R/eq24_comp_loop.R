#' Equation 24: Competition loop
#'
#' @param A.i.0 xxx
#' @param K.wg.0 xxx
#' @param y.i xxx
#' @param comp.df xxx
#' @param j xxx
#' @param i xxx
#' @param debug.continue xxx
#' @param internal.error.check xxx
#'
#' @return comp.list ...
#'
#' @export


eq24_comp_loop <- function(A.i.0,
                           K.wg.0 ,
                           y.i,
                           comp.df,
                           j,
                           i,
                           debug.continue = FALSE,
                           internal.error.check = FALSE
                           ){

  #if(i == 60){browser()}
  #number of iterations to loop
  ## Based on size of df used to track competition process
  ## Is artbitrary
  index.j <- 1:j

  # Assign intial states
  ## Initial number of settled birds
  ## When loop starts, no birds have settled on territories
  A.i.G.settled.tot.j <- rep(0, length(A.i.0))

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
    if(sum(A.i.active.j) <= K.wg.0 & j == 1){
      A.i.G.settled.tot.j <- A.i.0

      break
    }



          #store for reference; will be added to df used to track
          #competition process
          K.wg.open.j.init <- K.wg.open.j
          A.i.G.settled.tot.j.init <- sum(A.i.G.settled.tot.j)
          A.G.i.active.j.init  <- sum(A.i.active.j)

    ## settled best competitors to good habitat
    #A.i.settled.raw.j <- (A.i.active.j*y.i)/sum((A.i.active.j*y.i))*K.wg.open.j
    A.i.G.settled.raw.j <- eq24_competition(A.i.j = A.i.active.j,
                                            y.i = y.i,
                                            K.wg.open.j = K.wg.open.j)

    ## Trim ("correct") values if they exceed
    ## NOTE: this was originally set to use original abundance A.i.0
    ##
    ## A.i.G.settled.cor.j is the number that actually settled after all
    ## corrections/"trimmings" have been done
    A.i.G.settled.cor.j <- eq25_comp_constrain(A.i.G.settled.raw.j = A.i.G.settled.raw.j,
                                               A.i.active.j = A.i.active.j)

    ## Add those just settled to those already in good habitat
    #total settled    total settled         total settled
    #overwritten      this iteration        current
    A.i.G.settled.tot.j2a <- A.i.G.settled.cor.j + A.i.G.settled.tot.j

    #make sure that the total that have settle isn't more than total
    #available
    ###  Originally I did this as just a 1 step process
    ###  not 2 step
    ####   first, make sure more don't settle w/in a time j step than began the step
    ####   2nd, make sure the total number that have settle isn't greater than began
    ####        the overall time step of the model
    A.i.G.settled.tot.j2b <- ifelse(A.i.G.settled.tot.j2a >= A.i.0,
                                  A.i.0, A.i.G.settled.tot.j2a)

    ## Update carrying capacity
    ###   NOTE: use K.wg.0 NOT K.wg.open.j b/c A.i.G.settled.tot.j has
    ###         just been updated to reflect TOTAL number settled
    K.wg.open.j2 <- K.wg.0 - sum(A.i.G.settled.tot.j2b)

    ## calculate number remaining un-settledd
    # currently                       updated number
    # un-settled                       settled

    # Original when neg pop size  was occuring:
    #     A.i.unsettled.j <- A.i.active.j - A.i.G.settled.tot.j2b

    #this can be written 2 ways
    #1)
    #currently       starting  running total of all
    #unsettled     = number    settled so far
    A.i.unsettled.jvs1 <- A.i.0 - A.i.G.settled.tot.j2b

    #2)
    #currently         number active    number settled
    #unsettled     =   this time step   this time step
    A.i.unsettled.jvs2 <- A.i.active.j   - A.i.G.settled.cor.j

    #use vs 1
    A.i.unsettled.j <- A.i.unsettled.jvs1










    ## Bird unsettled at end of loop remain "Active" during next iteration
    A.i.active.j2 <- A.i.unsettled.j

            ### Save meta data
            comp.df$j[j] <- j
            comp.df$K.wg.open.j.init[j]         <- K.wg.open.j.init
            comp.df$K.wg.open.j.end[j]         <- K.wg.open.j
            comp.df$tot.settled.init[j]    <- A.i.G.settled.tot.j.init
            comp.df$tot.settled.final[j]   <- sum(A.i.G.settled.tot.j)
            comp.df$tot.active.init[j]     <- A.G.i.active.j.init
            comp.df$tot.active.final[j]    <- sum(A.i.active.j)
            comp.df$suc.settled.raw[j]    <- sum(A.i.G.settled.raw.j)
            comp.df$suc.settled.cor[j]    <- sum(A.i.G.settled.cor.j)
            comp.df$un.settled[j]         <- sum(A.i.unsettled.j)

            ### Test competition output
            if(internal.error.check == TRUE){
              if(any(A.i.G.settled.tot.j2a > A.i.0)){
                #message("Competition error in eq24_comp_loop: A.i.G > A.i.0 on iteration ", i)
                #browser()
              }

              if(sum(A.i.G.settled.tot.j2a) > K.wg.0){
                #message("Competition error eq24_comp_loop: sum(A.i.G) > K.wg on iteration ", i, " ",
                #        sum(A.i.G.settled.tot.j)," vs ",K.wg.0)
                #browser()
              }

              if(any(A.i.unsettled.j) < 0){
               # message("Competition error eq24_comp_loop: any(A.i.P) < 0 on iteration ", i, " ")
                browser()
              }

              if(any(A.i.active.j2) < 0){
               # message("Competition error eq24_comp_loop: any(A.i.P) < 0 on iteration ", i, " ")
                browser()
              }

            }


            # ## for looking at each step of output
            # compsum <- data.frame(A.i.0
            #                       ,A.i.G.settled.tot.j
            #                       ,A.i.active.j
            #                       ,A.i.G.settled.raw.j
            #                       ,A.i.G.settled.cor.j
            #                       ,per = round(A.i.G.settled.cor.j/A.i.active.j,2)
            #
            #                       ,A.i.G.settled.tot.j2a
            #                       ,A.i.G.settled.tot.j2b
            #                       ,A.i.unsettled.j
            #
            #                       ,A.i.active.j2)
            #
            # names(compsum) <- gsub("settled","set",names(compsum))
            # names(compsum) <- gsub("active","act",names(compsum))
            # names(compsum) <- gsub("A.i.G","",names(compsum))
            # names(compsum) <- gsub("A.i.","",names(compsum))
            #
            # compsum <- rbind(compsum,
            #                  sum = apply(compsum,2,sum))
            # compsum <- apply(compsum,2,round)


  ### update for next time step  ###
  A.i.active.j <- A.i.active.j2
  A.i.G.settled.tot.j <- A.i.G.settled.tot.j2b
  K.wg.open.j <- K.wg.open.j2

    #If number still active = 0, stop
    if(sum(A.i.active.j) == 0){ break }

    #If number settled = carrying capacity, stop
    if(round(sum(A.i.G.settled.tot.j),0) >= K.wg.0){ break }
  }

        ## Clean up tracking df
        ### remove blank rows from track df
        comp.df <- stats::na.omit(comp.df)

        ### round
        comp.df <- apply(comp.df,2,round)

  comp.list <- list(A.i.G.settled.tot.j = A.i.G.settled.tot.j,
                    comp.df = comp.df)

  if(debug.continue == TRUE){
    debugonce(runFAC)
  }

  return(comp.list)
}

