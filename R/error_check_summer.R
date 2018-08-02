#'
#' @export
#'
#'

error_check_summer <- function(B0,
                               W2,
                               P.breeding.pair.results,
                               check.errors.in,
                               i){
  ### Check B0 for errors
  error_check_B0(B0,
                 W2,
                 check.errors.in,
                 i = i)




  ### Create vector of equation names
  #### This is used for generation error messages
  P.equation.names       <- c("eq9_Pcgg", "eq10_Pcgp","eq11_Pcpg","eq12_Pcpp",
                              "eq13_Pkgg","eq14_Pkgp","eq15_Pkpg","eq16_Pkpp")

  names(P.equation.names) <- c("P.cgg", "P.cgp","P.cpg", "P.cpp",
                               "P.kgg", "P.kgp","P.kpg", "P.kpp")



  ### Check P.xxx for errors
  ###  Loops over

  ### Determine what to check
  #### Extract all "P" parameters from the check.errors.in arguement
  #### in main function call
  P.xxx.names    <- check.errors.in[grep("P.",check.errors.in)]
  P.xxx.values   <- P.breeding.pair.results[P.xxx.names]
  P.xxx.eq.names <- P.equation.names[P.xxx.names]

  ### Loop over parameters being checked
  if(length(P.xxx.names) > 0){
    for(p in 1:length(P.xxx.names)){
      error_check_Pxxx(P.xxx.name = P.xxx.names[p],
                       P.xxx.value = P.xxx.values[p],
                       P.xxx.eq.name = P.xxx.eq.names[p],
                       i = i)}
  }
}

