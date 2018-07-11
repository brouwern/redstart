#' Equation 24: competition - Allocation bird to high quality winter habitiat
#'
#' @details
#' A.i = initial population of vector when competition process starts
#' A.i.i = vector of individuals that haven't been allocated yet
#' A.G.i = vector of weights of individuals in A.i.i; this is constrainted so that A.G.i cannot be greater thatn A.i.i
#'        after constrain implemented, this is the vector of indiviuals alloacted
#'        to different habitat qualities
#' A.storage.i = vector to hold output of each loop
#' A.storage = object to hold entire output of competition process?
#'
#' @param K.wg Carrying capacity xxx
#' @param A.i xxx
#' @param gamma.i competition xxx
#'
#' @export

eq24compLoop <- function(K.wg,
                 A.i,
                 gamma.i){

  #how many birds have been allocated to high quality habitat so far?
  A.allocated.i <- 0
  names(A.allocated.i) <- "A.alloc"

  #How many territories remain open in high quality habitat
  K.wg.unallocated.i <- K.wg
  names(K.wg.unallocated.i) <- "K.alloc"


  #birds eneganging in compeition
  A.i.i <- A.i

  A.storage <- c(A.i.i,              #Pop vector
                 A.allocated.i,      #Numbre allocated so far
                 K.wg.unallocated.i) #number not yet allocated

  while(round(A.allocated.i) < round(K.wg)) #keepin allocating until the total allocated to good habitat equals the carrying capacity
  {

    #set current "weights" for birds that have not yet acquird a territory
    #The more competive a bird is, the higher its weight
    A.G.i <- (( A.i.i*gamma.i)/sum(A.i.i*gamma.i) )*K.wg.unallocated.i

    #EQUATION 25
    #constrain allocations
    A.G.i.pre <- A.G.i
    A.i.i.pre <- A.i.i
    A.G.i[A.G.i>A.i.i] <- A.i.i[A.G.i>A.i.i]  #if A.G. is ever > A.i, set A.G to be A.i
    #that is, if the weighting process
    #had made it so the A.G.i is greater than the actual number of available individuals
    #then A.G.i has to be re-set to be the number of actually available individusl (A.i.i)
    temp <- rbind(A.G.i.pre, A.G.i, A.i.i.pre, A.i.i)

    # temp$x <- NA
    # temp$x[A.G.i>A.i.i] <- "changes"
    #

    #How many birds have acquired territories?
    A.allocated.i <- sum(A.G.i) +
      A.allocated.i

    K.wg.unallocated.i <- K.wg.unallocated.i - sum(A.G.i)

    #Update A.i.i
    #How many birds still need to compete for high quality territories?
    A.i.i <- A.i.i - A.G.i

    A.storage.i <- c(A.G.i,
                     A.allocated.i,
                     K.wg.unallocated.i)

    A.storage <- rbind(A.storage,
                       A.storage.i)

    #if (A.allocated.i >=K.wg) break
    #if (sum(A.storage.i) == 0) break
  } #END while


  #Update A.G vector for output
  #  each A.allocated.i contains the output of 1 round of competition
  #  summing accross these yield then number of birds w/in each class that
  #  acuired high quality habitat
  #                                                "-1" drops initial row, which corresponds to A.storage?
  #                                                "-c(10,11)" drops last 2 columns, which are A.allocated.i and K.wg.unallocated.i

  #these if statements are required to determine how large the A.storage object is
  #if competition was resovled in a single round, the object will be just 2 rows in size
  #if competition took multiple rounds to resolve, the object will be larger

  if(dim(A.storage)[1] > 2) {A.G <- apply(A.storage[-1,-c(10,11)],2,sum)}
  if(dim(A.storage)[1] == 2){A.G <-       A.storage[-1,-c(10,11)]}



  #Allocation birds to low quality habitat
  A.P <- (A.i - A.G)

  #Return output
  A.post.comp <- list(A.G = A.G,
                      A.P = A.P,
                      A.allocated.i = A.storage)
}
