################################################################################
##### A linear model to calculate the slope that represent trend
################################################################################

#-------------------------------------------------------------------------------
#Arguments

#y: value information
#date: an integer that describe the day of the year

#-------------------------------------------------------------------------------
#Functions
fun_slope <- function(y, date) { 
  
  if(all(is.na(y))) {
    NA
  } else {
    m <- lm(y ~ date, na.action = na.omit); summary(m)$coefficients[2] 
  }
}
