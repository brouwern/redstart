#' FAC-IBM Step 3, Substep A1: Breeding habitat acquisition substep of pre-breeding step of breeding season using individual-based (IB) approach
#'
#'
#' @param W2 ...
#' @param K.bc ...
#' @param K.bk ...
#' @param ... additional arguements
#'
#' @return hab.aq.results.IB Results of habitatio acquisition
#'
#' @examples
#' #Set up param.set
#' param.set <- redstart::param_set()
#'
#' #set breeding carrying capacities to be relatively small
#' param.set$K.bc <- 50
#' param.set$K.bk <- 50
#'
#' #Set up W2 vector
#' W2 <- c(10,5,10,10); names(W2) <- c("mg","mp","fg","fp")
#'
#'
#' out <- step3_substepA1_hab_acquire_IB(W2 = W2,
#'            K.bc =  param.set$K.bc,
#'            K.bk = param.set$K.bk)
#'
#' summary(out)
#'
#' @export

step3_substepA1_hab_acquire_IB <- function(W2,
                                         K.bc,  #= param.set$K.bc,
                                         K.bk, # = param.set$K.bk,
                                         ...){

  ## Habitat ##
  #total amount of habitat
  K.total <- K.bc+K.bk

  #vector representing the quality of each habitat
  hab.b <- c(rep("bc",K.bc),rep("bk",K.bk))

  hab.b.IB <- data.frame(rank = 1:K.total,
                         hab.b = hab.b,
                         stringsAsFactors = F)


  ## MALES ###
  # vector representing the habitat where each male wintered
  #  length = total number of males

  if(W2["mg"] < 1 & W2["mp"] < 1){
    W2["mg"]  <- 1
    W2["mp"]  <- 1
    #message("male abundance less than 1")
  }
  hab.m.w <- c(rep("mg",W2["mg"]),rep("mp",W2["mp"]))

  male.IB <- data.frame(#sex = "m",
                        hab.male.w = hab.m.w,
                        rank = 1:length(hab.m.w),
                        #hab.b = NA,
                        stringsAsFactors = F)

  male.aquire <- merge(hab.b.IB,male.IB, all = TRUE)

  #male.aquire$hab.male.w[is.na(male.aquire$hab.b)] <- "floater"


  ## FEMALES ##
  # vector representing the habitat where each male wintered
  #  length = total number of males

  if(W2["fg"] < 1 & W2["fp"] < 1){
    W2["fg"]  <- 1
    W2["fp"]  <- 1
    #message("female abundance less than 1")
  }

  hab.f.w <- c(rep("fg",W2["fg"]),rep("fp",W2["fp"]))


  female.IB <- data.frame(#sex = "f",
    hab.femm.w = hab.f.w,
    rank = 1:length(hab.f.w),
    #hab.b = NA,
    stringsAsFactors = F)


  female.aquire <- merge(hab.b.IB,female.IB,all = TRUE)

  hab.aq.results.IB <- merge(male.aquire,
             female.aquire,
             all = T)




  hab.aq.results.IB$pair <- with(hab.aq.results.IB,
                                  paste(hab.b,hab.male.w,hab.femm.w,
                                        sep = "-"))

  # 1) empty territories
  hab.aq.results.IB$pair[grep("[b]..[N][A].[N][A]",
                               hab.aq.results.IB$pair)] <-"empty"

  # 2) males
  # 2a) males unpaired but with territory
  #males paired with "NA" = unpaired
  ## unpaired in source
  hab.aq.results.IB$pair[grep("^[b][c].[m]..[N][A]",
                               hab.aq.results.IB$pair)] <-"m-source-unpaired"
  ## unpaired in sink
  hab.aq.results.IB$pair[grep("[b][k].[m]..[N][A]",
                               hab.aq.results.IB$pair)] <- "m-sink-unpaired"

  # 2b) males unpaired AND no territory = floater
  hab.aq.results.IB$pair[grep("^[N][A].[m]..[N][A]",
                               hab.aq.results.IB$pair)] <-"m-floater"


  # femeales paried wiht "NA" disperse (effectivesly dead)
  ##  in source
  hab.aq.results.IB$pair[grep("[b][c].[N][A].[f].",
                               hab.aq.results.IB$pair)] <-"f-disperse"
  ##  in sink
  hab.aq.results.IB$pair[grep("[b][k].[N][A].[g][A]",
                               hab.aq.results.IB$pair)] <- "f-floater"

  hab.aq.results.IB$hab.b <- factor(hab.aq.results.IB$hab.b)
  hab.aq.results.IB$hab.male.w <- factor(hab.aq.results.IB$hab.male.w)
  hab.aq.results.IB$hab.femm.w <- factor(hab.aq.results.IB$hab.femm.w)
  hab.aq.results.IB$pair <- factor(hab.aq.results.IB$pair)


  #calcualte totals in each habitat
  B.md <- length(grep("m-floater",hab.aq.results.IB$pair))

  B.mc <- length(grep("bc-m",hab.aq.results.IB$pair))
  B.mk <- length(grep("bk-m",hab.aq.results.IB$pair))
  B.fc <- length(grep("bc-..-f.",hab.aq.results.IB$pair))
  B.fk <- length(grep("bk-..-f.",hab.aq.results.IB$pair))

  hab.aq.results.IB <- list(B.fc = B.fc,
                              B.fk = B.fk,
                              B.mc = B.mc,
                              B.mk = B.mk,
                              B.md = B.md,
                            hab.aq.results.IB = hab.aq.results.IB)


  return(hab.aq.results.IB)


}


