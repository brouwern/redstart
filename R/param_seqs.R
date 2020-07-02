#' Create sequence of parameter values between minimum and maximum.
#'
#' @param param.ranges minimum and maximum values for all parameters
#' @param len.out number of increments to divide range into
#'
#' @return param.seq List with 30 elements, one for each parameter.
#'
#'
#' @export

param_seqs <- function(param.ranges = param_ranges(),
                       len.out = 10){

  #create empty list to hold output
  NA.vector <- rep(NA, nrow(param.ranges))
  param.seqs <- as.list(NA.vector)
  names(param.seqs) <- row.names(param.ranges)


  #loop over df of ranges to generate sequences
  for(i in 1:length(param.seqs)){
    if(param.ranges$min[i] == param.ranges$max[i]){
      param.seqs[[i]]  <- param.ranges$min[i]
      next}
    param.seqs[[i]] <-  seq(from = param.ranges$min[i],
                            to =  param.ranges$max[i],
                            length.out = len.out)
  }


  return(param.seqs)
}


