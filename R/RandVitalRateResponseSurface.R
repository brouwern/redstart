#' Random Vital Rate Response surface
#'
#' This function sets up and runs a single iteration set models based on randomized vital rates
#' to see how Neq varies when summer and winter K are both varied
#' this produces a RESPONSE SURFACE
#' originally this function had a loop within it it to loop over multiple
#' sets of random vital rates but that has been placed external to this loop
#'
#' This function might actually be redundnat with the functions
#' it calls but Id need to check
#'
#' @param rand_vitals Random vital rates (?)
#' @param var.length. ???
#'
#' @export


RandVitalRateResponseSurface <- function(rand_vitals = r_vitals(n = 5),
                                         var.length. = 5){

  # Loop over each row in the df of random vital rates
  #for(j in 1:dim(rand_vitals)){

  # Set up parameters for using in the simulation
  # Carrying capacities are set to range from 1 to 1000 as in figure 28.3
  # of Runge & Marra
  # Key vital rates were generated rand_vitalsomly and are looped over
  #

  # print(j)

  #During each loop, the function xxxx [check - this has changed] is
  #called to generate input values to the fucntion makeparam.griddf  simulate accross
  #values that will vary during the simulations are given a min and
  #and a max, while those that are fixed just have the
  #same value repeated twice
  #the values that are given a min and max are typically just the carrying capacities
  #K.bc and K.wg, but can also be gamma (winter competition)
  #and co (carry over)
  #Vital rates and other parameters are set to be constant
  #by just using rep() to replicate the same value twice

  min.max.rand_vitals <- param_ranges(  #
    K.bc.   = c(1,1000)  #breeding capacity is in PAIRS
    ,K.wg.   = c(1,3000)  #winter capacity is in INDIVIDUALS
    #,gamma.  = c(1,5)
    ,co.      = c(1,1)
    #Winter SURVIVAL (S.w)
    ,S.w.mg. = rep(rand_vitals$S.w.fg,2)
    ,S.w.mp. = rep(rand_vitals$S.w.fp,2)
    ,S.w.fg. = rep(rand_vitals$S.w.fg,2)
    ,S.w.fp. = rep(rand_vitals$S.w.fp,2)

    #Spring (Northward) migration SURVIVAL (S.m)
    ,S.m.mg. = rep(rand_vitals$S.m.fg,2)
    ,S.m.mp. = rep(rand_vitals$S.m.fp,2)
    ,S.m.fg. = rep(rand_vitals$S.m.fg,2)
    ,S.m.fp. = rep(rand_vitals$S.m.fp,2)

    #FECUNDITIES
    ,R.base.rate. = rep(rand_vitals$R.base.rate,2)
    ,R.hab.effect. = rep(rand_vitals$R.hab.effect,2)   #effects of pair breeding in poor habitat
    #,co. = c(1,1)    #2              #magnitude of carry over effect; c =1 equals no carry over

    #sex ratio
    #,f. = c(0.50,0.50)

    #Breeding season mortality
    ,S.b.mc. =  rep(rand_vitals$S.b.fc,2)#males in sour.c.e habitat
    ,S.b.mk. =  rep(rand_vitals$S.b.fk,2) #males in sin.k. habitat
    #,S.b.md. = c(0.80,0.80) #"drain" males
    ,S.b.fc. = rep(rand_vitals$S.b.fc,2)
    ,S.b.fk. = rep(rand_vitals$S.b.fk,2)

    # FALL (southward) MIGRATION Mortality of ADULTS
    #pg 381; Table 28.2
    ,S.f.mc. = rep(rand_vitals$S.f.fc,2) #males from sour.c.e
    ,S.f.mk. = rep(rand_vitals$S.f.fk,2)
    ,S.f.md. = rep(rand_vitals$S.f.fc,2) #drain males have higher surv because they dont have costs of repro
    ,S.f.fc. = rep(rand_vitals$S.f.fc,2)
    ,S.f.fk. = rep(rand_vitals$S.f.fk,2)

    # FALL MIGRATION Survival of OFFSPRING
    ,S.y.mc. = rep(rand_vitals$S.y.fc,2)
    ,S.y.mk. = rep(rand_vitals$S.y.fk,2)
    ,S.y.fc. = rep(rand_vitals$S.y.fc,2)
    ,S.y.fk. = rep(rand_vitals$S.y.fk,2)
  )


  #Generate the range of values that will be used for the simulations
  #A parameter given a min and a max by param_range (check this)
  #will generate a sequence of values that takes on the number of different
  #values declared by var.length =
  i.full.range.rand_vitals <- param_seqs(param.ranges = min.max.rand_vitals, #previousluy makeparam.griddf()
                                         len.out = var.length.)

  
  ##??????
  ##????? need to add call to param_grid() here I think

  
  #Run model
  iraw.MultiFac.out <- runFAC_multi(param.grid = i.full.range.rand_vitals)

  #perturb Kbc
  dKbc.full.range.rand_vitals       <- i.full.range.rand_vitals
  dKbc.full.range.rand_vitals$K.bc <- i.full.range.rand_vitals$K.bc+(i.full.range.rand_vitals$K.bc/10)
  dKbc.MultiFac.out <- runFAC_multi(param.grid = dKbc.full.range.rand_vitals)

  #perturb Kwg
  dKwg.full.range.rand_vitals       <- i.full.range.rand_vitals
  dKwg.full.range.rand_vitals$K.wg <- i.full.range.rand_vitals$K.wg+(i.full.range.rand_vitals$K.wg/10)
  dKwg.MultiFac.out <- runFAC_multi(param.grid = dKwg.full.range.rand_vitals)


  # summary(lm(B.tot ~ K.bc.i*K.wg.i, data = i.MultiFac.out))
  #
  # plotFig28.3(i.MultiFac.out,plot = "P")
  # plotFig28.3(i.MultiFac.out,plot = "C")




  #}#close for loop

  return(list(orig = iraw.MultiFac.out,
              perturb.Kbc = dKbc.MultiFac.out,
              perturb.Kwg = dKwg.MultiFac.out))

}#close function
