# Generate random life histories/vital rates
r_vitals <- function(d = 0.25,   #sets range of data to draw random values from
                            n = 10000,                     #number of simulated life histories
                            params. = param_ranges() #starting values for parameters; originally makeParamCombos.df()
){

  # Generate initial values for random variables
  #   these intial values are extracted from the defualts
  #   contained withitn the function makeParamCombos.df()
  #   which are by default saved to the object params.
  #   when makeRandomVital() is called
  # params.init <- params.[,c("S.b.fc","S.f.fc","S.w.fg","S.m.fg",
  #                           "S.y.fc","R.base.rate","f")]
  #


  params.init <- params.[   ,c("S.w.fg","S.w.fp"
                               ,"S.m.fg","S.m.fp"
                               ,"S.b.fc","S.b.fk"
                               ,"S.f.fc","S.f.fk"
                               ,"S.y.fc","S.y.fk"
                               ,"R.base.rate"
                               ,"R.hab.effect"
                               ,"f")]

  #create min and max values to use for random number generation
  params.min   <- params.init-params.init*d
  params.min$f <- 0.5
  params.max   <- params.init+params.init*d
  params.max$f <- 0.5

  #fix survival params so they dont exceed 1
  i.surv.params <- grep("^S",names(params.max))
  params.max[i.surv.params] <- ifelse(params.max[i.surv.params]>1,
                                      0.999,
                                      params.max[i.surv.params])




  # Generate random data
  px <- runif(n = length(params.min)*n,
              min = as.numeric(params.min),
              max = as.numeric(params.max))
  px <- matrix(px, nrow = n,byrow = T)
  px <- data.frame(px)
  names(px) <- names(params.init)


  # Check to make sure parameters in source habitat are not
  # less than parameters in sink habitat
  #browser()
  i.bad0 <- with(px, which(S.w.fg <=  S.w.fp))#
  i.bad1 <- with(px, which(S.m.fg <= S.m.fp))
  i.bad2 <- with(px, which(S.b.fc <= S.b.fk))
  i.bad3 <- with(px, which(S.f.fc <= S.f.fk))
  i.bad4 <- with(px, which(S.y.fc <= S.y.fk))

  # fecundities?
  #R.base.rate vs R.hab.effect?  I dont think this needs
  # to be checked...


  i.bad <- unique(c(i.bad0,i.bad1,i.bad2,i.bad3,i.bad4))
  length(i.bad)

  px <- px[-i.bad,]
  dim(px)

  #calcualte demography summary stats for high quality habitat


  #high quality habitat base rates
  S.ad.hi <- with(params.init, S.b.fc*S.f.fc*S.w.fg*S.m.fg)
  S.juv.hi <- with(params.init,S.y.fc*       S.w.fg*S.m.fg)
  Fec.hi <- with(params.init, R.base.rate)
  Lamb.hi <- S.ad.hi+ S.juv.hi*Fec.hi*px$f[1]

  #low quality habitat base rates
  S.ad.lo  <- with(params.init, S.b.fk*S.f.fk*S.w.fp*S.m.fp)
  S.juv.lo <- with(params.init, S.y.fk*       S.w.fp*S.m.fp)
  Fec.lo   <- with(params.init, R.base.rate*R.hab.effect)
  Lamb.lo  <- S.ad.lo+ S.juv.lo*Fec.lo*px$f[1]

  #high quality habitat rand data
  px$S.ad.hi  <- with(px, S.b.fc*S.f.fc*S.w.fg*S.m.fg)
  px$S.juv.hi <- with(px, S.y.fc*       S.w.fg*S.m.fg)
  px$Fec.hi   <- with(px, R.base.rate)
  px$Lamb.hi  <- px$S.ad.hi+ px$S.juv.hi*px$Fec.hi*px$f
  hist(px$Lamb.hi )
  abline(v = Lamb.hi,col = 2)

  #low quality habitat rand data
  px$S.ad.lo  <- with(px, S.b.fk*S.f.fk*S.w.fp*S.m.fp)
  hist(px$S.ad.lo);abline(v = S.ad.lo,col = 2)

  px$S.juv.lo <- with(px, S.y.fk*       S.w.fp*S.m.fp)
  hist(px$S.juv.lo);abline(v = S.juv.lo,col = 2)

  px$Fec.lo   <- with(px, R.base.rate*R.hab.effect)
  hist(px$Fec.lo);abline(v = Fec.lo,col = 2)

  hist(px$S.ad.lo +  px$S.juv.lo*px$Fec.lo*px$f )

  px$Lamb.lo  <- px$S.ad.lo+ px$S.juv.lo*px$Fec.lo*px$f
  hist(px$Lamb.lo)
  abline(v=Lamb.lo, col = 2)

  with(px, scatter.smooth(Lamb.hi ~ Lamb.lo))
  abline(a = 0, b = 1)
  with(px, scatter.smooth(S.b.fc ~ S.b.fk))
  abline(a = 0, b = 1)
  with(px, scatter.smooth(S.y.fc ~ S.y.fk))
  abline(a = 0, b = 1)

  with(px, hist(Lamb.hi))
  with(px, hist(Lamb.lo))

  #extract values approx equal to average lambda from R&M
  i.1.06  <- which(round(px$Lamb.hi,1) == round(Lambda(params.init),1))
  i.0.624 <- which(round(px$Lamb.lo,1) == round(Lamb.lo,1))



  px2 <- px[intersect(i.1.06,i.0.624),]

  with(px2, plot(Lamb.hi ~ Lamb.lo))
  abline(a = 0, b = 1)
  with(px2, scatter.smooth(S.b.fc ~ S.b.fk))
  abline(a = 0, b = 1)
  with(px2, scatter.smooth(S.y.fc ~ S.y.fk))
  abline(a = 0, b = 1)


  return(px2)

}

