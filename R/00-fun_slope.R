################################################################################
##### A linear model to calculate the slope that represent trend
################################################################################

#-------------------------------------------------------------------------------
#Arguments

#' @param y value information
#' @param date an integer that describe the day of the year

#-------------------------------------------------------------------------------
#Functions
fun_slope <- function(x, doy) {

  if (length(x[x = !is.na(x)]) < 3) {
    NA_real_
    
  } else {
    
    lm(tanh(x^2) ~ doy)$coefficients[[2]]
    
  }
}
