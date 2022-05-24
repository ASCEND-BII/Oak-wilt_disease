################################################################################
#### Synthetic Spectral Angle Mapper
################################################################################

#-------------------------------------------------------------------------------
#Library
library(Rcpp)
library(data.table)

#-------------------------------------------------------------------------------
# C++ code load

cppFunction("
            double sam(NumericVector& x, NumericVector& y) {
            const double conv = 180/3.141593;
            double normEM = sum(pow(y, 2));
            double radians = acos(sum(x * y) / sqrt(sum(pow(x,2)) * normEM));
            double angle = (radians * conv);
            double simiarity = (90 - angle) / 90;
            return simiarity;
            }", plugins=c("cpp11")
            )

#-------------------------------------------------------------------------------
#Arguments

#' @param target target spectra to mixing
#' @param to_mixing spectra to mixing with the target spectra
#' @param fraction faction of target spectra that will have the mixing spectra
#' @param ite_by_fractions number of iterations by fractions

data <- fread("data/spectral_library.csv")
data <- subset(data, QAI != 0)
data <- data[, c(4, 8:17)]

# 8 "BLUE", 9 "GREEN", 10 "RED", 11 "REDEDGE1", 12 "REDEDGE2", 13, "REDEDGE3", 
# 14 "BROADNIR", 15 "NIR", 16 "SWIR1", 17 "SWIR2"  

# (SWIR1 - NIR) / (SWIR1 + NIR) 
# (REDEDGE3 / REDEDGE1) - 1

#-------------------------------------------------------------------------------
#Function
synthetic_sam <- function(data, target = "wilted", fractions, ite_by_fractions = 100) {
  
  #frame to return
  spectral_mixing <- data.table()
  
  #Add shadows
  #shadows <- as.data.table(matrix(c(rep(0, ncol(data))), nrow = 1))
  #colnames(shadows) <- colnames(data)
  #data <- rbind(data, shadows)
  
  #Unique classes
  unique_endmembers <- unique(data$Endmember)
  target_frame <- subset(data, Endmember == target)
  unique_endmembers <- unique_endmembers[unique_endmembers != target]
  
  #Progress bar
  pb <- txtProgressBar(min = 1, max = length(fractions), style = 3)
  
  #Loop over fractions
  for(i in 1:length(fractions)) {
    
    #Progress
    setTxtProgressBar(pb, i)
    
    #Target samples to use
    target_sample <- sample(nrow(target_frame), ite_by_fractions, replace = TRUE)
    
    for(ii in 1:ite_by_fractions) {
      
      #Select_endmember
      select_endmember <- sample(length(unique_endmembers), 1)
      sub_data <- subset(data, Endmember == unique_endmembers[select_endmember])
      to_mixing_sample <- sub_data[sample(nrow(sub_data), 1), ]
      
      #Endmembers
      endmember1 <- as.matrix(target_frame[target_sample[ii], -1])
      endmember2 <- as.matrix(to_mixing_sample[, -1])
      
      #Fractions and Mixing
      frac1 <- endmember1 * fractions[i]
      frac2 <- endmember2 * (1-fractions[i])
      mixing <- frac1 + frac2
      
      #SAM
      sam_fraction <- sam(endmember1[1,], mixing[1,])
      
      mixing <- cbind(SAM = sam_fraction, 
                      mixing)
      
      #Return
      spectral_mixing <- rbind(spectral_mixing, mixing)
      
    }
  }
  
  return(spectral_mixing)
  
}

#END
fractions <- seq(0, 1, 0.1)
results <- synthetic_sam(data, target = "wilted", fractions, ite_by_fractions = 100)
results <- na.exclude(results)
hist(results$SAM)
fwrite(results, "data/synthetic_sam.csv")


results <- synthetic_sam(data, target = "wilted", fractions, ite_by_fractions = 100)
fwrite(results, "data/wilted_ssam.csv")

results <- synthetic_sam(data, target = "healty", fractions, ite_by_fractions = 100)
fwrite(results, "data/healty_ssam.csv")

results <- synthetic_sam(data, target = "dead", fractions, ite_by_fractions = 100)
fwrite(results, "data/dead_ssam.csv")
