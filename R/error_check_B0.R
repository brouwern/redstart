#' QA/QC: Check for errors in B0 (vector of abundances after breeding habiatat acquisition)
#'
#' @param B0 5 x 1 vector representing population state after breeding habitat acqusitions
#' @param W2 4 x 1 vectir representing populati state after northward spring migration
#' @param check.errors.in vector of model components to check for errors during a given run
#' @param i Current iteration of runFAC()
#' @param ... additional parameters
#'
#' @examples
#' B0 <- c(10,20,30,40,50)
#' W2 <- c(10,20,30,40)
#' error_check_B0(B0 = B0,
#' W2 = W2,
#' check.errors.in = "B0",
#' i = 1)
#'
#' @export


error_check_B0 <- function(B0,
                           W2,
                           check.errors.in,
                           i, ...)
if("B0"  %in% check.errors.in){



  error.msg <- paste("Element of B0 does not equal an element of W2 on iteration",i)

  ## Total of W2 b/f habitat aquisition = total of B0 after
  if(floor(sum(W2)) != floor(sum(B0))){
    message(error.msg)
  }

  ## Total of m in W2 == total males in B0
  if(floor(sum(W2[1:2])) !=  floor(sum(B0[1:3]))){
    message(paste(error.msg),"-males")
  }

  ## Total of m in W2 == total males in B0
  if(floor(sum(W2[3:4])) !=  floor(sum(B0[4:5]))){
    message(paste(error.msg),"-females")
  }
}
