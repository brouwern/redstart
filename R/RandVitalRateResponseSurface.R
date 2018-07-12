
#This function sets up and runs a single iteration set models based on randomized vital rates
#to see how Neq varies when summer and winter K are both varied
#this produces a RESPONSE SURFACE
#originally this function had a loop within it it to loop over multiple
#sets of random vital rates but that has been placed external to this loop
#
#This function might actually be redundnat with the functions
# it calls but Id need to check

#Function was originally in a script with a "temp_" prefix
#and was then rename "7_vita_rate_sim2.R"
#was then moved to the master set of sub-functions
#"1FAC-subfnxnsVS2.R" on "Thu Jul 21 10:27:22 2016"


RandVitalRateResponseSurface <- function(randVitals.df = makeRandomVital(n = 5),
                                         var.length. = 5){

  # Loop over each row in the df of random vital rates
  #for(j in 1:dim(randVitals.df)){

  # Set up parameters for using in the simulation
  # Carrying capacities are set to range from 1 to 1000 as in figure 28.3
  # of Runge & Marra
  # Key vital rates were generated randVitals.dfomly and are looped over
  #

  # print(j)

  #During each loop, the function makeParamRanges.df() is
  #called to generate input values to the fucntion makeParamCombos.df  simulate accross
  #values that will vary during the simulations are given a min and
  #and a max, while those that are fixed just have the
  #same value repeated twice
  #the values that are given a min and max are typically just the carrying capacities
  #K.bc and K.wg, but can also be gamma (winter competition)
  #and co (carry over)
  #Vital rates and other parameters are set to be constant
  #by just using rep() to replicate the same value twice

  min.max.randVitals.df <- makeParamRanges.df(
    K.bc.   = c(1,1000)  #breeding capacity is in PAIRS
    ,K.wg.   = c(1,3000)  #winter capacity is in INDIVIDUALS
    #,gamma.  = c(1,5)
    ,co      = c(1,1)
    #Winter SURVIVAL (S.w)
    ,S.w.mg. = rep(randVitals.df$S.w.fg,2)
    ,S.w.mp. = rep(randVitals.df$S.w.fp,2)
    ,S.w.fg. = rep(randVitals.df$S.w.fg,2)
    ,S.w.fp. = rep(randVitals.df$S.w.fp,2)

    #Spring (Northward) migration SURVIVAL (S.m)
    ,S.m.mg. = rep(randVitals.df$S.m.fg,2)
    ,S.m.mp. = rep(randVitals.df$S.m.fp,2)
    ,S.m.fg. = rep(randVitals.df$S.m.fg,2)
    ,S.m.fp. = rep(randVitals.df$S.m.fp,2)

    #FECUNDITIES
    ,R.base.rate. = rep(randVitals.df$R.base.rate,2)
    ,R.hab.effect. = rep(randVitals.df$R.hab.effect,2)   #effects of pair breeding in poor habitat
    #,co. = c(1,1)    #2              #magnitude of carry over effect; c =1 equals no carry over

    #sex ratio
    #,f. = c(0.50,0.50)

    #Breeding season mortality
    ,S.b.mc. =  rep(randVitals.df$S.b.fc,2)#males in sour.c.e habitat
    ,S.b.mk. =  rep(randVitals.df$S.b.fk,2) #males in sin.k. habitat
    #,S.b.md. = c(0.80,0.80) #"drain" males
    ,S.b.fc. = rep(randVitals.df$S.b.fc,2)
    ,S.b.fk. = rep(randVitals.df$S.b.fk,2)

    # FALL (southward) MIGRATION Mortality of ADULTS
    #pg 381; Table 28.2
    ,S.f.mc. = rep(randVitals.df$S.f.fc,2) #males from sour.c.e
    ,S.f.mk. = rep(randVitals.df$S.f.fk,2)
    ,S.f.md. = rep(randVitals.df$S.f.fc,2) #drain males have higher surv b/c they dont have costs of repro
    ,S.f.fc. = rep(randVitals.df$S.f.fc,2)
    ,S.f.fk. = rep(randVitals.df$S.f.fk,2)

    # FALL MIGRATION Survival of OFFSPRING
    ,S.y.mc. = rep(randVitals.df$S.y.fc,2)
    ,S.y.mk. = rep(randVitals.df$S.y.fk,2)
    ,S.y.fc. = rep(randVitals.df$S.y.fc,2)
    ,S.y.fk. = rep(randVitals.df$S.y.fk,2)
  )


  #Generate the range of values that will be used for the simulations
  #A parameter given a min and a max by makeParamRanges.df
  #will generate a sequence of values that takes on the number of different
  #values declared by var.length =
  i.full.range.randVitals.df <- makeParamCombos.df(paramRanges = min.max.randVitals.df,
                                                   var.length = var.length.)
  #browser()
  #Run model
  iraw.MultiFac.out <- runMultiFAC(paramCombos. = i.full.range.randVitals.df)

  #perturb Kbc
  dKbc.full.range.randVitals.df       <- i.full.range.randVitals.df
  dKbc.full.range.randVitals.df$K.bc <- i.full.range.randVitals.df$K.bc+(i.full.range.randVitals.df$K.bc/10)
  dKbc.MultiFac.out <- runMultiFAC(paramCombos. = dKbc.full.range.randVitals.df)

  #perturb Kwg
  dKwg.full.range.randVitals.df       <- i.full.range.randVitals.df
  dKwg.full.range.randVitals.df$K.wg <- i.full.range.randVitals.df$K.wg+(i.full.range.randVitals.df$K.wg/10)
  dKwg.MultiFac.out <- runMultiFAC(paramCombos. = dKwg.full.range.randVitals.df)


  # summary(lm(B.tot ~ K.bc.i*K.wg.i, data = i.MultiFac.out))
  #
  # plotFig28.3(i.MultiFac.out,plot = "P")
  # plotFig28.3(i.MultiFac.out,plot = "C")




  #}#close for loop

  return(list(orig = iraw.MultiFac.out,
              perturb.Kbc = dKbc.MultiFac.out,
              perturb.Kwg = dKwg.MultiFac.out))

}#close function
