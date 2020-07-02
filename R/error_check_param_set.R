#' QA/QC: Check that input into runFAC() is correct size
#'
#' This function is used for Quality control on initial parameters entering runFAC().
#'
#' @param param.set Set of parameters for a single run of runFAC()
#'
#' @return error message
#'
#' @export

error_check_param_set <- function(param.set){
  ## Check input:
  ### check that that only a single vector of parameters have been sent
  if(nrow(param.set) > 1){
    message("Paramter matrix is ", dim(param.set))
    message("Incorrect param matrix size.",
            "\nFunction runFAC() accepts only a single row of parameters at a time")

    stop()
  }
}
