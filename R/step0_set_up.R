#' FAC Step 0: Set up survival matrices & other preliminaries
#'
#' @param param.set Initial parameters and population state for a single run of model to equilibirum
#' @param iterations Number of iterations to run model to allow it to reach equilibrium
#' @param use.IBM Use individual-based modeling implementation of breeding territory aquisition and mate pairing
#' @param use.IBM.S.b Used individual-based modeling implementation of breeding survibal.
#'
#' @examples
#'
#' # Initialize model
#' runFAC.i <- step0_set_up()
#'
#' # Look at structure of list
#' ## Elements for model output, matrices, parameters, etc
#' str(runFAC.i,1)
#'
#' # Look at top of dataframe to hold output
#' head(runFAC.i$FAC.out.RM)[,1:5]
#'
#' # Look at 2nd matrix used in model, winter survival
#' runFAC.i$param.matrices[2]
#' runFAC.i$param.matrices$S.w
#'
#' @return runFAC.i list ...
#'
#' @export



step0_set_up <- function(param.set = param_set(),
                         iterations = 150,
                         use.IBM = F,
                         use.IBM.S.b = F){


  # Check main parameter dataframe
  ## Check that the initial dataframe of parameters is correct size
  error_check_param_set(param.set)

  #if use.IBM.S.b set to true, set run.IBM = TRUE als
  if(use.IBM.S.b == T){
    use.IBM <- T
  }


  ## Create vector to hold number of male birds on good breeding territor
  ### used for diagnostics to determine if equilibirum has been reached
      ## TO DO: make into function
      ##        set up for both Runge-Marra and IBM
  W.mg.diagnostic.df <- data.frame(i = 1:iterations,
                            W.mg = NA,
                            W.mg.lambda = NA,
                            W.mg.lambda.mean = NA,
                            W.mg.lambda.var = NA)


  #########################################
  ## Set up matrices of fixed parameter ###
  #########################################

  ### Calculate Competition for winter territories: gamma
  gamma.i <- eq24_make_gamma_vec(param.set$gamma)


  ### EQUATION 2: winter survival matrix
  ### Winter SURVIVAL (S.w) of birds in different habitat qualities

  S.w <- eq02buildW1Mat(param.set$S.w.mg,
                           param.set$S.w.mp,
                           param.set$S.w.fg,
                           param.set$S.w.fp)

  ### EQUATION 3: spring migration survival matrix
  ### Northward migration survival (S.m)

  S.m <- eq03buildW2Mat(param.set$S.m.mg,
                             param.set$S.m.mp,
                             param.set$S.m.fg,
                             param.set$S.m.fp)


  ### Equation 20: Breeding season survival matrix

  S.b <- eq20buildSbmat(param.set$S.b.mc,
                             param.set$S.b.mk,
                             param.set$S.b.md,
                             param.set$S.b.fc,
                             param.set$S.b.fk)


  #EQUATION 21: Spring migration survival matrix = adults

  S.f <-  eq21buildSfmat(param.set$S.f.mc,
                              param.set$S.f.mk,
                              param.set$S.f.md,
                              param.set$S.f.fc,
                              param.set$S.f.fk)



  ### EQUATION 22 Spring mgiraiton survival matrix - young

  S.y <- with(param.set,
              eq22buildSymat(S.y.mc,
                                S.y.mk,
                                S.y.fc,
                                S.y.fk))

  R.all <- eq18AbuildRvec(param.set$R.base.rate,
                               param.set$R.hab.effect,
                               param.set$co)

  #make.F.matrix
  f.mat <- eq19AbuildFmat(param.set$f)


  ### matrcies into list
  ###  List of all survival matrices & other parameter matrices
  param.matrices <- list(gamma.i = gamma.i,
                         S.w = S.w, S.m = S.m,
                         S.b = S.b, S.f = S.f,
                         S.y = S.y,
                         R.all = R.all,
                         f.mat = f.mat)

  ### All prep material into a list
  runFAC.i <- list(FAC.out.RM = make_FAC_df(iterations),
                   FAC.out.IB = make_FAC_df(iterations),
                   FAC.eq.state.RM = NA,
                   FAC.eq.state.IB = NA,
                   W.mg.diagnostic.df = W.mg.diagnostic.df,
                   param.matrices = param.matrices,
                   params.initial = param.set,
                   use.IBM = use.IBM)

  return(runFAC.i)

}


