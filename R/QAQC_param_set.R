#' Quality control on parameters entering runFAC()
#'
#' @export

QAQC_param_set <- function(param.set){
  ## Check input:
  ### check that that only a single vector of parameters have been sent
  if(nrow(param.set) > 1){
    message("Paramter matrix is ", dim(param.set))
    message("Incorrect param matrix size.",
            "\nFunction runFAC() accepts only a single row of parameters at a time")

    break()
  }
}
