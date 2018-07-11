QAQC_paramsWorking <- function(paramsWorking){
  ## Check input:
  ### check that that only a single vector of parameters have been sent
  if(nrow(paramsWorking) > 1){
    message("Paramter matrix is ", dim(paramsWorking))
    message("Incorrect param matrix size.",
            "\nFunction runFAC() accepts only a single row of parameters at a time")

    break()
  }
}
