#' Make parameter range for looping over multiple values of a parameter
#'
#' @param rangeParam ...
#' @param var.length ...
#'
#' @return param.seq Dataframe with 2 columns, minimum and maxium values for each parameter
#'
#' @export

makeParamSeq <- function(rangeParam,
                         var.length){
  if(rangeParam[1]==rangeParam[2]){param.seq <- rangeParam[1]}
  if(rangeParam[1]<rangeParam[2]){param.seq <-   seq(from = rangeParam[1],
                                                     to=rangeParam[2],
                                                     length.out = var.length)}
  return(param.seq)
}
