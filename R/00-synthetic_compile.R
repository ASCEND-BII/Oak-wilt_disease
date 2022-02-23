################################################################################
#' @title Compile synthetic mixing results from FORCE
################################################################################

#-------------------------------------------------------------------------------
# Library
library(data.table)

#-------------------------------------------------------------------------------
# Arguments 
#' @param path Path of the synthetic mixing results
path <- "/media/antonio/Work/Oak-wilt/synthetic/mixtures"

#-------------------------------------------------------------------------------
# Data processing

#-------------------------------------------------------------------------------
# Function

compiled_mixing <- function(path) {
  
  #List of files
  files <- dir(path)
  
  #Frame to compile
  frame <- data.table(files = files)
  
  #Class of files
  frame[, type := strsplit(files, "_")[[1]][2], 
        by = seq_len(nrow(frame))]
  
  #Files to compile
  responce <- subset(frame, type == "RESPONSE")
  features <- subset(frame, type == "FEATURES")
  
  #Complete
  frame_responce <- data.table()
  frame_features <- data.table()
  
  #Loop over responce
  for(i in 1:nrow(responce)) {
    
    #File arguments
    name <- responce$files[i]
    mixing <- as.numeric(substr(name, 1, 3))
    class <- as.numeric(substr(name, 24, 26))
    iteration <- as.numeric(substr(name, 38, 40))
    
    #Read
    file <- fread(paste0(path, "/", name))
    colnames(file) <- "fraction"
    
    arguments <- cbind(class = class,
                       mixing = mixing,
                       iteration = iteration,
                       sample = 1:nrow(file),
                       file)
    
    frame_responce <- rbind(frame_responce,
                            arguments)
    
  }
  
  #Loop over features
  for(i in 1:nrow(features)) {
    
    #File arguments
    name <- features$files[i]
    mixing <- as.numeric(substr(name, 1, 3))
    class <- as.numeric(substr(name, 24, 26))
    iteration <- as.numeric(substr(name, 38, 40))
    
    #Read
    file <- fread(paste0(path, "/", name))
    colnames(file) <- c("BLUE", "GREEN", "RED", "REDEDGE1", 
                        "REDEDGE2", "REDEDGE3", "NIR",	"SWIR1",	"SWIR2")
    
    arguments <- cbind(class = class,
                       mixing = mixing,
                       iteration = iteration,
                       sample = 1:nrow(file),
                       file)
    
    frame_features <- rbind(frame_features,
                            arguments)
    
  }
  
  #Merge files
  compiled <- merge(frame_responce, frame_features, 
                    by = c("class", "mixing", "iteration", "sample"))
}

compiled <- compiled_mixing(path)
fwrite(compiled, "data/synthetic_mixing.csv") #Three options

synt_spectra <- compiled[class == 1]
synt_spectra <- synt_spectra[, c(5:14)]
colnames(synt_spectra)[1] <- "SAM"
fwrite(synt_spectra, "data/healty_ssam.csv")

synt_spectra <- compiled[class == 2]
synt_spectra <- synt_spectra[, c(5:14)]
colnames(synt_spectra)[1] <- "SAM"
fwrite(synt_spectra, "data/wilted_ssam.csv")

synt_spectra <- compiled[class == 3]
synt_spectra <- synt_spectra[, c(5:14)]
colnames(synt_spectra)[1] <- "SAM"
fwrite(synt_spectra, "data/dead_ssam.csv")
