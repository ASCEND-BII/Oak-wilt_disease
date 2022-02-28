################################################################################
##### A linear model to calculate the slope that represent trend
################################################################################

#-------------------------------------------------------------------------------
#Arguments

#' @param y value information
#' @param date an integer that describe the day of the year

#-------------------------------------------------------------------------------
#Functions
fun_slope <- function(y, date) { 
  
  if(all(is.na(y))) {
    NA
  } else {
    
    #object to return
    r <- rep(NA, 3)
    
    #linear models
    m <- lm(healty ~ date, na.action = na.omit)
    
    r[1] <- summary(m)$coefficients[2] #slope
    r[2] <- summary(m)$coefficients[1] #sd slope
    r[3] <- summary(m)$r.squared #r2
    
    return(r[1])
  }
}
