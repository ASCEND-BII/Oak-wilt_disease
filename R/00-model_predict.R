model_predict <- function(scene, models) {
  
  predicted <- list()
  
  for(k in 1:length(models)) {
    
    repetition <- predict(scene, model = models[[k]], 
                          type = "prob", cores = 1, 
                          na.rm = TRUE, cpkgs = c("caret"))
    
    predicted[[k]] <- repetition
    names(predicted)[[k]] <- names(models)[k]
    
  }
  
  #Stack
  for(k in 1:length(models)) {
    
    if(k == 1) {
      
      Healthy <- predicted[[k]]$Healthy
      Symptomatic <- predicted[[k]]$Symptomatic
      Dead <- predicted[[k]]$Dead
      
    }
    
    Healthy <- c(Healthy, predicted[[k]]$Healthy)
    Symptomatic <- c(Symptomatic, predicted[[k]]$Symptomatic)
    Dead <- c(Dead, predicted[[k]]$Dead)
    
  }
  
  #Remove
  rm(list = c("predicted"))
  
  #Get mean
  Healthy_mean <- app(Healthy, fun= "mean")
  Symptomatic_mean <- app(Symptomatic, fun = "mean")
  Dead_mean <- app(Dead, fun = "mean")
  
  #Get sd
  Healthy_sd <- app(Healthy, fun= "sd")
  Symptomatic_sd <- app(Symptomatic, fun = "sd")
  Dead_sd <- app(Dead, fun = "sd")
  
  #Predicted
  predicted <- c(Healthy_mean, Symptomatic_mean, Dead_mean)
  uncertainty <- c(Healthy_sd, Symptomatic_sd, Dead_sd)
  
  names(predicted) <- c("Healthy", "Symptomatic", "Dead")
  names(uncertainty) <- c("Healthy", "Symptomatic", "Dead")
  
  #Remove potential garbage
  rm(list = c("Healthy_mean", "Symptomatic_mean", "Dead_mean",
              "Healthy_sd", "Symptomatic_sd", "Dead_sd"))
  
  return(list(Predicted = predicted,
              Uncertainty = uncertainty))
  
}
