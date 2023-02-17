datasplit_tile <- function(frame, training = 0.6) {
  
  frame <- frame
  frame$sample <- 1:nrow(frame)
  tiles <- unique(frame$tile)
  to_training <- as.numeric()
  
  for(i in 1:length(tiles)) {
    
    x_tile <- subset(frame, tile == tiles[i])
    min_samples <- min(table(x_tile$Condition))
    min_samples <- floor(min_samples * training)
    
    healthy <- subset(x_tile, Condition == "Healthy")
    symptomatic <- subset(x_tile, Condition == "Symptomatic")
    dead <- subset(x_tile, Condition == "Dead")
    
    healthy <- healthy[sample(1:nrow(healthy), min_samples), sample]
    symptomatic <- symptomatic[sample(1:nrow(symptomatic), min_samples), sample]
    dead <- dead[sample(1:nrow(dead), min_samples), sample]
    
    to_training <- c(to_training, healthy, symptomatic, dead)
    
  }
  
  return(to_training)
  
}