#' Set minimum and maximum values for all parameters.
#'
#' Generates a dataframe defining the minimum and maximum values of each parameter.
#' This dataframe of minimums and maximums is then turned into a list if scalers (single numbers) for parameters that do not vary and
#' and vectors for numbers that do vary by the function param_seqs().  A dataframe of all possible parameter combinatnsion (aka, a parameter grid) is then made with param_grid().
#'
#' Default values are based on Runage and Marra (2004).  The arguments "scenario" and "figure" can be used to set up the parameters for specific figures from Runge and Marra (2004).
#'
#'
#' @param scenario Different habitat scenarios defined by Runge & Marra 2004.  In particular sets carrying capacity for breeding source habitat (K.bc) and winter good habitat (K.wg)    If set overides other varibles that may have been set using other arguements.
#' @param figure Generate figure.  Currently only figures 28.3 and 28.4 are implemented
#' @param gamma. Strength of male dominance.
#' @param c. Strength of winter to breeding season carry over.
#' @param K.bc. Carrying capacity (K) of source (high quality) breeding habitat.
#' @param K.bk. Carrying capacity (K) of sink (low quality) breeding habitat.
#' @param K.wg. Carrying capacity (K) of winter good (high quality) habitat.
#' @param S.w.mg. Survival (S) during winter (.w.) of males (.m) in good habitat (g).  Note that there is no age structure in winter.
#' @param S.w.mp. Survival (S) during winter (.w.) of males (.m) in poor habitat (p).
#' @param S.w.fg. Survival (S) during winter (.w.) of females (.f) in good habitat (g).
#' @param S.w.fp. Survival (S) during winter (.w.) of females (.f) in good habitat (p).
#' @param S.m.mg. Survival (S) during spring migration (.m.) of males (.m) originating from good (g) winter habitat.
#' @param S.m.mp. Survival (S) during spring migration (.m.) of males (.m) originating from poor (p) winter habitat.
#' @param S.m.fg. Survival (S) during spring migration (.m.) of females (.f) originating from good (g) winter habitat.
#' @param S.m.fp. Survival (S) during spring migration (.m.) of females (.m) originating from poor (p) winter habitat.
#' @param R.base.rate. Fecundity base rate
#' @param R.hab.effect.  Effect on fecundity of a pair breeding in poor habitat
#' @param co.  Magnitude of carry over effect; c = 1 is no carry over
#' @param f.  Sex ratio
#' @param S.b.mc. Survival (S) during breeding (.b.) of males (.m) in source (c) habitat (source = good habitat)
#' @param S.b.mk. Survival (S) during breeding (.b.) of males (.m) in sink (k) habitat (sink = poor habitat)
#' @param S.b.md. Survival (S) during breeding (.b.) of males (.m) with drain (d) status (drain = floater or unpaired)
#' @param S.b.fc. Survival (S) during breeding (.b.) of females (.f) in source (c) habitat (source = good habitat)
#' @param S.b.fk. Survival (S) during breeding (.b.) of females (.f) in sink (k) habitat (sink = poor habitat)
#' @param S.f.mc. Survival (S) during fall (.f.) migration of males (.m) originating from source (c) habitat
#' @param S.f.mk. Survival (S) during fall (.f.) migration of males (.m) originating from sink (k) habitat
#' @param S.f.md. Survival (S) during fall (.f.) migration of males (.m) with drain (d) status (aka floater, unpaired)
#' @param S.f.fc. Survival (S) during fall (.f.) migration of females (.f) originating from source (c) habitat
#' @param S.f.fk. Survival (S) during fall (.f.) migration of females (.f) originating from sink (k) habitat
#' @param S.y.mc. Survival (S) during fall migration of young (.y.) males (.m) originating from source (c) habitat
#' @param S.y.mk. Survival (S) during fall migration of young (.y.) males (.m) originating from sink (k) habitat
#' @param S.y.fc. Survival (S) during fall migration of young (.y.) females (.f) originating from source (c) habitat
#' @param S.y.fk. Survival (S) during fall migration of young (.y.) females (.f) originating from sink (k) habitat
#' @param verbose T/F return messages
#'
#' @return param.ranges 30 x 2 dataframe of the min. and max. parameter values that will be considered.  Often values are fixed and do not vary and so are listed as both min and max.
#'
#' @references Runge, MC and PP Marra.  2004.  Modeling seasonal
#'       interactions in the population dynamics of migratory birds.
#'       In Greenberg, R and PP Marra, eds.  Birds of two worlds: The ecology & evolution of migration.
#'       Johns Hopkins University Press, Baltimore.
#'
#' @examples
#'
#' # Set parameters for the winter limited scenario in original paper (Runge & Marra 2004)
#' param.ranges <- param_ranges(scenario = "winter.limited")
#' head(param.ranges)
#'
#' # Parameters to remake Figure 28.3 in original paper (Runge & Marra 2004)
#' param.ranges <- param_ranges(figure = 28.3)
#' param.ranges[c("K.bc","K.wg"), ]
#'
#' # Setting carrying capacities to 0 causes errors when running model
#' ## param_ranges() therefore automatically changes to prevent this bug
#' param.ranges <- param_ranges(K.bc. = c(0,100))
#' param.ranges[c("K.bc","K.wg"), ]
#'
#' # Set up different scenarios
#' ## this is similar to data for figure 28.5, but without gamma varying
#' winter.lim <- param_ranges(scenario = "winter")
#' intermed <- param_ranges(scenario = "intermediate")
#' summer.lim <- param_ranges(scenario = "summer")
#'
#' # Show data
#' ## Note only 1st 6 rows shown; delete "[1:6,]" to show all.
#' data.frame(winter.lim, intermed, summer.lim)[1:6,]
#'
#' @export

param_ranges <- function(
  scenario = NA
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
  ,R.base.rate.  = c(1.8,1.8)
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
  ,S.f.md. = c(0.80,0.80) #drain males have higher surv because they don't have costs of repro
  ,S.f.fc. = c(0.80,0.80)
  ,S.f.fk. = c(0.75,0.75)

  #FALL MIGRATION Survival of OFFSPRING
  ,S.y.mc. = c(0.80,0.80)
  ,S.y.mk. = c(0.75,0.75)
  ,S.y.fc. = c(0.80,0.80)
  ,S.y.fk. = c(0.75,0.75)
  ,verbose = TRUE){


  #Set scenarios from Runge and Marra

  ## Winter-limited scanario
  if(is.na(scenario) == FALSE){
    if(scenario%in% c("winter.limited",
                       "winter.lim",
                       "winter",
                       "win.lim",
                       "win")){
      K.bc. <- c(800,800)
      K.wg. <- c(485,485)
      if(verbose == TRUE){
        message("Setting breeding 'source' carrying capacity K.bc to 800",
              "\nSetting winter   'good'   carrying capacity K.wg to 484")
      }
    }

    ## Intermediate scanario
    if(scenario %in% c("intermediate","inter","both","intermed","int")){
      K.bc. <- c(224,224)
      K.wg. <- c(580,580)
      if(verbose == TRUE){
        message("Setting breeding 'source' carrying capacity K.bc to 224",
                "\nSetting winter   'good'   carrying capacity K.wg to 580")
      }
    }

    ## Summer-limited scanario
    if(scenario %in% c("summer.limited","summer.lim","summer","sum.lim","sum")){
      K.bc. <-  c(205,205)
      K.wg. <- c(900,900)

      if(verbose == TRUE){
        message("Setting breeding 'source' carrying capacity K.bc to 205",
                "\nSetting winter   'good'   carrying capacity K.wg to 900")
      }
    }}


  #Set up for particular figures
  if(is.na(figure)== FALSE){

    ## Figure 28.3: 3D surface plot
    if(figure == 28.3){
      K.bc.  <- c(1,1000) # varies
      K.wg.  <- c(1,1000) # varies
    }

    ## Figure 28.4: vary breeding carrying capacity
    if(figure == 28.4){
      K.bc. <- c(1,600)
      K.wg.  <- c(900,900)
    }

    ## Figure 28.5: vary male dominance parameter gamma
    if(figure == 28.5){
      gamma. <- c(1,5)
      if(verbose == TRUE){
        message("\nSetting male winter dominance parameter to vary from 1 to 5")

        if(is.na(scenario) == TRUE){
          message(
          "\nNOTE: A population limitation 'scenario' was not set",
          "\nFor remaking figure 28.5 create seperate parameter grids with gamma varying for",
          "\nthe 3 population limitation scenarios",
          "\n(arguement scenario = 'summer', 'intermediate', 'winter')",
          "\nSet the arguement 'verbose = FALSE' to turn off this message")
        }
      }
    }

    ## Figure 28.6: vary carry over effect
    if(figure == 28.6){
      message("Setting carry over effect to vary from 1 to 2")
      co. <- c(1,2)
    }
  }


  #check for zeros in carrying capacities
  #should add message about this: 0s can result in model not running so they
  # are changed in to 1
  min.K <- min(c(K.bc.,K.bk.,K.wg.))

  if(min.K == 0){
    message("One of your carrying capacities is set to 0, which due to an unresolved bug can cause problems.  This value has been set to 1.  Sorry.")
    K.bc.[1] <- ifelse(K.bc.[1] == 0, 1, K.bc.[1])  #Change any zeros to 1 to make model avoid errors
    K.bc.[2] <- ifelse(K.bc.[2] == 0, 1, K.bc.[2])
    K.bk.[1] <- ifelse(K.bk.[1] == 0, 1, K.bk.[1])  #Change any zeros to 1 to make model avoid errors
    K.bk.[2] <- ifelse(K.bk.[2] == 0, 1, K.bk.[2])
    K.wg.[1] <- ifelse(K.wg.[1] == 0, 1, K.wg.[1])  #Change any zeros to 1 to make model avoid errors
    K.wg.[2] <- ifelse(K.wg.[2] == 0, 1, K.wg.[2])
  }



  #Ranges of parametrs to consider
  param.ranges <- rbind(
    # scenario = scenario I would like to indicate if a scenario was used
    # this could be added as an attribute perhaps?  or use a list?
    gamma  = gamma.
    ,co.      = co.
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
    ,S.f.md = S.f.md. #drain males have higher surv because they don't have costs of repro
    ,S.f.fc = S.f.fc.
    ,S.f.fk = S.f.fk.

    #FALL MIGRATION Survival of OFFSPRING
    ,S.y.mc = S.y.mc.
    ,S.y.mk = S.y.mk.
    ,S.y.fc = S.y.fc.
    ,S.y.fk = S.y.fk.)

  param.ranges <- data.frame(param.ranges)
  names(param.ranges) <- c("min","max")
  return(param.ranges)
}

