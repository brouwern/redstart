#' Make paramater ranges
#'
#' Generates a dataframe defining the minimum maximum values of each parameter;
#' these are then turned into a matrix of all combinations by the function
#' makeParamCombos.df()
#'
#' @return Dataframe with 2 columns, minimum and maxium values for each parameter
#'
#' @param scenario Difference scenarios defined by Runge & Marra
#' @param figure generate figure.  Currently only figures 28.3 and 28.4 are implemented
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
#' @return param.ranges 30 x 2 dataframe of the min. and max. parameter values that will be considered.  Often values are fixed.
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal
#'       interactions in the population dynamics of migratory birds.
#'       In Greenberg, R and PP Marra, eds.  Birds of two worlds.
#'       Johns Hopkins University Press, Baltimore.
#'
#' @examples
#' param.ranges <- makeParamRanges.df(scenario. = "winter.limited")
#' head(param.ranges)
#'
#' param.ranges <- makeParamRanges.df(figure = 28.3)
#' param.ranges[c("K.bc","K.wg"), ]
#'
#' @export

makeParamRanges.df <- function(
  scenario. = NA
  ,figure = NA
  ,gamma.  = c(5,5)
  ,c.      = c(1,1)
  ,K.bc.   = c(1000,1000)                #c(0,600)
  ,K.bk.   = c(10000,10000)
  ,K.wg.   = c(900,900)

  #Winter SURVIVAL (S.w)
  ,S.w.mg. = c(0.80,0.80) #c(0.80,0.80)
  ,S.w.mp. = c(0.80,0.80)
  ,S.w.fg. = c(0.80,0.80)
  ,S.w.fp. = c(0.80,0.80)

  #Spring (Northward) migration SURVIVAL (S.m)
  ,S.m.mg. = c(0.90,0.90)  #do they call this s.smg?
  ,S.m.mp. = c(0.80,0.80)
  ,S.m.fg. = c(0.90,0.90)
  ,S.m.fp. = c(0.80,0.80)

  #FECUNDITIES
  ,R.base.rate. = c(1.8,1.8)
  ,R.hab.effect. = c(0.5,0.5)   #effects of pair breeding in poor habitat
  ,co. = c(1,1)    #2              #magnitude of carry over effect; c =1 equals no carry over

  #sex ratio
  ,f. = c(0.50,0.50)

  #Breeding season mortality
  ,S.b.mc. = c(0.95,0.95) #males in sour.c.e habitat
  ,S.b.mk. = c(0.85,0.85) #males in sin.k. habitat
  ,S.b.md. = c(0.80,0.80) #"drain" males
  ,S.b.fc. = c(0.95,0.95)
  ,S.b.fk. = c(0.85,0.85)

  #FALL (southward) MIGRATION Mortality of ADULTS
  #pg 381; Table 28.2
  ,S.f.mc. = c(0.80,0.80) # c(0.80,0.80)#males from sour.c.e
  ,S.f.mk. = c(0.75,0.75) #c(0.75,0.75)
  ,S.f.md. = c(0.80,0.80) #drain males have higher surv b/c they don't have costs of repro
  ,S.f.fc. = c(0.80,0.80)
  ,S.f.fk. = c(0.75,0.75)

  #FALL MIGRATION Survival of OFFSPRING
  ,S.y.mc. = c(0.80,0.80)
  ,S.y.mk. = c(0.75,0.75)
  ,S.y.fc. = c(0.80,0.80)
  ,S.y.fk. = c(0.75,0.75)){


  #Set scenarios from Runge and Marra
  if(is.na(scenario.) == FALSE){
    if(scenario.%in% c("winter.limited",
                       "winter.lim",
                       "winter",
                       "win.lim",
                       "win")){
      K.bc. <- c(800,800)
      K.wg. <- c(485,485)
    }

    if(scenario. %in% c("intermediate","inter","both","intermed")){
      K.bc. <- c(224,224)
      K.wg. <- c(580,580)
    }

    if(scenario. %in% c("summer.limited","summer.lim","summer","sum.lim","sum")){
      K.bc. <-  c(205,205)
      K.wg. <- c(900,900)
    }}


  #Set up for particular figures
  if(is.na(figure)== FALSE){
    if(figure == 28.4){
      K.bc. <- c(1,600)
      K.wg.  <- c(900,900)
    }
    if(figure == 28.3){
      K.bc. <- c(1,1000)
      K.wg.  <- c(1,1000)
    }
  }

  #Ranges of parametrs to consider
  param.ranges <- t(data.frame(
    row.names = c("min","max")
    # scenario = scenario. I would like to indicate if a scenario was used
    # this could be added as an attribute perhaps?  or use a list?
    ,gamma  = gamma.
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

    #FALL MIGRATION Survival of OFFSPRING
    ,S.y.mc = S.y.mc.
    ,S.y.mk = S.y.mk.
    ,S.y.fc = S.y.fc.
    ,S.y.fk = S.y.fk.))

  return(param.ranges)
}

