#'
#'
#' @examples
#'
#' # Generate default matrices
#' runFAC.i <- runFAC_set_initial_params()
#'
#' # Look at structure of list
#' str(runFAC.i)
#'
#' # Look at top of dataframe to hold output
#' head(runFAC.i$FAC.out)[,1:5]
#'
#' # Look at 2nd matrix, winter survival
#' runFAC.i$param.matrices[2]
#' runFAC.i$param.matrices$S.w
#' @export



runFAC_set_up <- function(param.set = param_set(),
                                      iterations = 150,
                                      use.IBM = F){


  # Check main parameter dataframe
  ## Check that the initial dataframe of parameters is correct size
  error_check_param_set(param.set)

  #if use.IBM set to true, set run.IBM = TRUE also
  if(use.IBM == T){
    run.IBM <- T
  }


  ## Create vector to hold number of male birds on good breeding territor
  ### used for diagnostics to determine if equilibirum has been reached
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

  S.w <- with(param.set,
              eq02bulidMat(S.w.mg,
                           S.w.mp,
                           S.w.fg,
                           S.w.fp))

  ### EQUATION 3: spring migration survival matrix
  ### Northward migration survival (S.m)

  S.m <- with(param.set,
              eq03(S.m.mg,
                   S.m.mp,
                   S.m.fg,
                   S.m.fp))


  ### Equation 20: Breeding season survival matrix

  S.b <- with(param.set,
              eq20_build_Sb_mat(S.b.mc,
                                S.b.mk,
                                S.b.md,
                                S.b.fc,
                                S.b.fk))


  #EQUATION 21: Spring migration survival matrix = adults

  S.f <-  with(param.set,
               eq21_build_Sf_mat(S.f.mc,
                                 S.f.mk,
                                 S.f.md,
                                 S.f.fc,
                                 S.f.fk))



  ### EQUATION 22 Spring mgiraiton survival matrix - young

  S.y <- with(param.set,
              eq22_build_Sy_mat(S.y.mc,
                                S.y.mk,
                                S.y.fc,
                                S.y.fk))

  R.all <- with(param.set,
                eq18buildRmat(R.base.rate,
                              R.hab.effect,
                              co))

  #make.F.matrix
  f.mat <- with(param.set,
                     eq19buildFmat(f))



  # List of all survival and othe parameter matrices
  param.matrices <- list(gamma.i = gamma.i,
                         S.w = S.w, S.m = S.m,
                         S.b = S.b, S.f = S.f,
                         S.y = S.y,
                         R.all = R.all,
                         f.mat = f.mat)


  runFAC.i <- list(FAC.out.RM = make_FAC_df(iterations),
                   FAC.out.IB = make_FAC_df(iterations),
                   FAC.eq.state.RM = NA,
                   FAC.eq.state.IB = NA,
                   W.mg.diagnostic.df = W.mg.diagnostic.df,
                   param.matrices = param.matrices,
                   params.initial = param.set
                   )

  return(runFAC.i)

}


