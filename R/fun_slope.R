################################################################################
##### A linear model to calculate the slope that represent trend
################################################################################

#-------------------------------------------------------------------------------
#Arguments

#y: value information
#Date: an integer that describe the Julian day

#-------------------------------------------------------------------------------
#Functions
fun_slope <- function(y) { 
  
  if(all(is.na(y))) {
    NA
  } else {
    m <- lm(y ~ julian_day, na.action = na.omit); summary(m)$coefficients[2] 
  }
}
