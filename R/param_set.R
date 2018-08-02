#' Make paramater ranges
#'
#' Generates a dataframe defining the minimum maximum values of each parameter;
#' these are then turned into a matrix of all combinations by the function
#' makeParamCombos.df()
#'
#'
#' @param scenario Difference scenarios defined by Runge & Marra: "winter" limited, "intermediate", and "spring" limited.  If set overides other varibles.
#' @param figure deprecated
#' @param gamma. xx
#' @param c. xxx
#' @param K.bc. xxx
#' @param K.bk. xxx
#' @param K.wg. xxx
#' @param S.w.mg. xxx
#' @param S.w.mp. xxx
#' @param S.w.fg. xxx
#' @param S.w.fp. xxx
#' @param S.m.mg. do they call this s.smg?
#' @param S.m.mp. xxx
#' @param S.m.fg. xxx
#' @param S.m.fp. xxx
#' @param R.base.rate. xxx
#' @param R.hab.effect.  effects of pair breeding in poor habitat
#' @param co.  magnitude of carry over effect; c 1 equals no carry over
#' @param f.  sex ratio
#' @param S.b.mc.  males in sour.c.e habitat
#' @param S.b.mk.  males in sin.k. habitat
#' @param S.b.md.  "drain" males
#' @param S.b.fc. xxx
#' @param S.b.fk. xxx
#' @param S.f.mc. males from sour.c.e
#' @param S.f.mk. xxx
#' @param S.f.md. drain males have higher surv b/c they don't have costs of repro
#' @param S.f.fc. xxx
#' @param S.f.fk. xxx
#' @param S.y.mc. xxx
#' @param S.y.mk. xxx
#' @param S.y.fc. xxx
#' @param S.y.fk. xxx
#'
#' @return param.ranges 30 x 2 dataframe of the min. and max. parameter values that will be considered.  Often values are fixed and do not vary.
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal
#'       interactions in the population dynamics of migratory birds.
#'       In Greenberg, R and PP Marra, eds.  Birds of two worlds: The ecology & evolution of migration.
#'       Johns Hopkins University Press, Baltimore.
#'
#' @examples
#'
#' # Default values
#' ## Print out default values (only 1st 10 show.  Remove "[1:10]" to see all)
#' param_set()[1:10]
#'
#' ## Print defaults in a column by taking the transpose using t()
#' ## (only 1st 10 show.  Remove "[1:10]" to see all)
#' t(param_set()[1:10])
#'
#' # Set parameters for the different scenarios plotted in figure Figure 28.5 in Runge & Marra (2004)
#' ## (note that to actually replicate Fig 28.5 param_rangs() and param_seqs() need to be used)
#' ## to set up full parameter grid with the male dominanc parameter gamma varying.
#'
#' ## winter limited
#' winter <- param_set(scenario = "winter.limited")
#'
#' ## intermediate scenario
#' intermediate <- param_set(scenario = "intermediate")
#'
#' ## summer limited scenario
#' summer <- param_set(scenario = "summer.limited")
#'
#' ## compare the scenarios
#' ### K.bc varies
#' ### k.bk fixed
#' ### K.wg varies
#' rbind(winter = winter, intermed = intermediate, summer = summer)[, 1:10]
#'
#'
#' @export

param_set <- function(
   scenario = NA
  ,figure = NA
  ,gamma.  = 5
  ,c.      = 1
  ,K.bc.   = 1000                #c(0,600)
  ,K.bk.   = 10000
  ,K.wg.   = 900

  #Winter SURVIVAL (S.w)
  ,S.w.mg. = 0.80
  ,S.w.mp. = 0.80
  ,S.w.fg. = 0.80
  ,S.w.fp. = 0.80

  #Spring (Northward) migration SURVIVAL (S.m)
  ,S.m.mg. = 0.90  #do they call this s.smg?
  ,S.m.mp. = 0.90
  ,S.m.fg. = 0.90
  ,S.m.fp. = 0.90

  #FECUNDITIES
  ,R.base.rate. = 1.8
  ,R.hab.effect. = 0.5  #effects of pair breeding in poor habitat
  ,co. = 1   #2         #magnitude of carry over effect; c =1 equals no carry over

  #sex ratio
  ,f. = 0.50

  #Breeding season mortality
  ,S.b.mc. = 0.95 #males in sour.c.e habitat
  ,S.b.mk. = 0.85 #males in sin.k. habitat
  ,S.b.md. = 0.80 #"drain" males
  ,S.b.fc. = 0.95
  ,S.b.fk. = 0.85

  #FALL (southward) MIGRATION Mortality of ADULTS
  #pg 381; Table 28.2
  ,S.f.mc. = 0.80 # c(0.80,0.80)#males from sour.c.e
  ,S.f.mk. = 0.75 #c(0.75,0.75)
  ,S.f.md. = 0.80#drain males have higher surv b/c they don't have costs of repro
  ,S.f.fc. = 0.80
  ,S.f.fk. = 0.75

  #FALL MIGRATION Survival of OFFSPRING
  ,S.y.mc. = 0.80
  ,S.y.mk. = 0.75
  ,S.y.fc. = 0.80
  ,S.y.fk. = 0.75){


  #Set scenarios from Runge and Marra
  if(is.na(scenario) == FALSE){
    if(scenario%in% c("winter.limited",
                       "winter.lim",
                       "winter",
                       "win.lim",
                       "win")){
      K.bc. <- c(800)
      K.wg. <- c(485)
    }

    if(scenario %in% c("intermediate","inter","both","intermed")){
      K.bc. <- c(224)
      K.wg. <- c(580)
    }

    if(scenario %in% c("summer.limited","summer.lim","summer","sum.lim","sum")){
      K.bc. <-  c(205)
      K.wg. <- c(900)
    }}



  #check for zeros in carrying capacities
  #should add messge about this: 0s can result in model not running so they
  # are changed in to 1
  min.K <- min(c(K.bc.,K.bk.,K.wg.))

  if(min.K == 0){
    message("One of your carrying capacities is set to 0, which due to an unresolved bug can cause problems.  This value has been set to 1.  Sorry.")
    K.bc. <- ifelse(K.bc. == 0, 1, K.bc.)  #Change any zeros to 1 to make model avoid errors
    K.bk. <- ifelse(K.bk. == 0, 1, K.bk.)  #Change any zeros to 1 to make model avoid errors
    K.wg. <- ifelse(K.wg. == 0, 1, K.wg.)  #Change any zeros to 1 to make model avoid errors

  }



  #Ranges of parametrs to consider
  param.set <- data.frame(
    # scenario = scenario I would like to indicate if a scenario was used
    # this could be added as an attribute perhaps?  or use a list?
    gamma  = gamma.
    ,co      = co.
    ,K.bc   = K.bc.                #c(0,600)
    ,K.bk   = K.bk.
    ,K.wg   = K.wg.

    #Winter SURVIVAL (S.w)
    ,S.w.mg = S.w.mg.
    ,S.w.mp = S.w.mp.
    ,S.w.fg = S.w.fg.
    ,S.w.fp = S.w.fp.

    #Spring (Northward) migration SURVIVAL (S.m)
    ,S.m.mg = S.m.mg.  #do they call this s.smg?
    ,S.m.mp = S.m.mp.
    ,S.m.fg = S.m.fg.
    ,S.m.fp = S.m.fp.

    #FECUNDITIES
    ,R.base.rate = R.base.rate.
    ,R.hab.effect = R.hab.effect.   #effects of pair breeding in poor habitat

    #sex ratio
    ,f = f.

    #Breeding season mortality
    ,S.b.mc = S.b.mc. #males in sour.c.e habitat
    ,S.b.mk = S.b.mk. #males in sin.k. habitat
    ,S.b.md = S.b.md. #"drain" males
    ,S.b.fc = S.b.fc.
    ,S.b.fk = S.b.fk.

    #FALL (southward) MIGRATION Mortality of ADULTS
    #pg 381; Table 28.2
    ,S.f.mc = S.f.mc. #males from sour.c.e
    ,S.f.mk = S.f.mk.
    ,S.f.md = S.f.md. #drain males have higher surv b/c they don't have costs of repro
    ,S.f.fc = S.f.fc.
    ,S.f.fk = S.f.fk.

    #FALL MIGRATION Survival of OFFSPRINGpa
    ,S.y.mc = S.y.mc.
    ,S.y.mk = S.y.mk.
    ,S.y.fc = S.y.fc.
    ,S.y.fk = S.y.fk.)


  return(param.set)
}

