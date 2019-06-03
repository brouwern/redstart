#' Create sequence of parameter values between minimum and maximum.  Output is a list
#'
#' @param param.ranges minimum and maximum values for all parameters, generted by [param_range]
#' @param length.out number of increments to divide range into
#'
#' @return param.seq List with 30 elements, one for each parameter.
#'
#' @examples
#' #generate min and max values for each parameter used for figure 28.3 in original paper
#' params <- param_ranges(figure = 28.3)
#'
#' # Generate parameter sequence
#' params.seqs <- param_seqs(param.ranges = params)
#'
#'
#' ## Most are constants; 3rd and 5th element vary accross a range of values
#' params.seqs$K.wg
#'
#' @export

param_seqs <- function(param.ranges = param_ranges(),
                       length.out = 10){

  #create empty list to hold output
  NA.vector <- rep(NA, nrow(param.ranges))
  param.seqs <- as.list(NA.vector)
  names(param.seqs) <- row.names(param.ranges)


  #loop over df of ranges to generate sequences
  for(i in 1:length(param.seqs)){
    if(param.ranges$min[i] == param.ranges$max[i]){
      param.seqs[[i]]  <- param.ranges$min[i]
      next}
    param.seqs[[i]] <-  seq(param.ranges$min[i],
                              param.ranges$max[i],
                              length.out = length.out)
  }


  return(param.seqs)
}


