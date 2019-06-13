#' FAC step 1: Execute 1st step of Runge & Marra FAC model: overwinter survival
#' 1.RM and W1.IB, the pop stucture after overwinter survival
#'
#' @param i xxx
#' @param runFAC.i xxx
#' @param use.IBM Use individual based model submodel
#' @param Ninit Initial population size
#' @param W.list.RM xxx
#' @param W.list.IB xxx
#'
#' @return W1.list
#'
#' @examples
#'
#' # Initial population size for iteration 1
#' ## step1_winter_survival(i = 1, use.IBM = F,Ninit = c(10,0,10,0))
#' ## step1_winter_survival(i = 1, use.IBM = T,Ninit = c(10,0,10,0))
#'
#'
#' @export
#'


step1_winter_survival <- function(i,
                                  runFAC.i,
                                   use.IBM,
                                   Ninit,
                                   W.list.RM = NULL,
                                   W.list.IB = NULL){

  ##################################
  ### Step 1: winter survival    ###
  ##################################

  ############################################
  ### Step 1a: create winter pop vector    ###
  ############################################

  ##################
  ### EQUATION 1 ###
  ##################

  #### Create vector W0 of population state at end of winter
  ##### (W.xx are the output of the final step of each iteration of the model)


  #### 1st iteration only:
  ##### Initial winter population state from runFAC() function call

  # set W0.RM and W0.IB to NULL in case not being used
  ## eg, if IBM is not being used create NULL object
  ## as a place holder in the list that is output
  W1.RM <- NULL
  W1.IB <- NULL

  if(i == 1){

    # Populate list w/ initial population sizes from main function call
    W.list <- list(W.mg = Ninit[1]
                   ,W.mp = Ninit[2]
                   ,W.fg = Ninit[3]
                   ,W.fp = Ninit[4])

    # Create vector W0
    W0.RM <- eq01buildW0vect(W.list$W.mg,
                             W.list$W.mp,
                             W.list$W.fg,
                             W.list$W.fp)


    if(use.IBM == TRUE){
      # Create copy of vector W0 for use with individual based (IB) model
      W0.IB <- W0.RM
    }

  }

  #### All iterations except the 1st
  if(i > 1){

    W0.RM <- eq01buildW0vect(W.list.RM$W.mg,
                             W.list.RM$W.mp,
                             W.list.RM$W.fg,
                             W.list.RM$W.fp)

    if(use.IBM == TRUE){
      W0.IB <- eq01buildW0vect(W.list.IB$W.mg,
                               W.list.IB$W.mp,
                               W.list.IB$W.fg,
                               W.list.IB$W.fp)

    }
  }




  ###################
  ### error check ###
  ###################

  # if(check.errors == TRUE & any(W0.RM < 0)){
  #
  #   err.msg <- paste("element of W0 < 0 on iteration ",i)
  #   message(err.msg)
  #   error.log <- list(param.set = param.set,
  #                     error.msg = err.msg)
  #   save(error.log,file = "./error_log/param_set_neg_popsize.RData")
  #
  # }

  ###################################
  ### Step 1b: winter survival    ###
  ###################################

  ### Winter SURVIVAL (S.w) of birds in different habitat qualities
  #### Calculcate Population state at end of winter

  W1.RM <- runFAC.i$param.matrices$S.w %*% W0.RM


  if(use.IBM == T){
    W1.IB <- runFAC.i$param.matrices$S.w %*% W0.IB
  }



  W1.list <- list(W1.RM = W1.RM,
                  W1.IB = W1.IB)

  return(W1.list)

}


