#' Plot time series of a single run of a FAC model
#'
#' @export
#'

plot_runFAC <- function(out.df){
  graphics::par(mfrow = c(1,2))
  graphics::plot(out.df$tot.W ~ out.df$t,
       xlab = "Iteration",
       ylab = "Total winter  population",
       main = "Total population size")
  graphics::points(out.df$tot.B ~ out.df$t,  #why *2?
         col = 2,
         xlab = "Iteration",
         ylab = "Total adult breeding population")

  graphics::plot(out.df$lambda ~ out.df$t,
       xlab = "Iteraction",
       ylab = "Population growth (lambda)",
       main = "Population growth")
}
