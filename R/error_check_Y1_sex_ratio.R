#' Check offspring sex ratio
#'
#' @param Y1 Vector of offspring abundance
#' @param i current iteration
#' @param ... ...
#'
#' @export

error_check_Y1_sex_ratio <- function(Y1, i,...){


  if( floor(Y1["mc"]) != floor(Y1["fc"])){

    message("Error in source habitat offspring sex ratio on iteration ",i,
            "; Y1 = ", Y1)
  }

  if(floor(Y1["mk"]) != floor(Y1["fk"])){

    message("Error in sink habitat offspring sex ratio on iteration ",i,
            "; Y1 = ", Y1)
  }

}
