model_predict <- function(scene, models, threads = 4) {
  
  predicted <- list()
  
  for(k in 1:length(models)) {
    
    repetition <- predict(scene, model = models[[k]], 
                          type = "prob", cores = threads, 
                          na.rm = TRUE, cpkgs = c("caret"))
    
    predicted[[k]] <- repetition
    names(predicted)[[k]] <- names(models)[k]
    
  }
  
  #Stack
  for(k in 1:length(models)) {
    
    if(k == 1) {
      
      Healthy <- predicted[[k]]$Healthy
      Wilted <- predicted[[k]]$Wilted
      Dead <- predicted[[k]]$Dead
      
    }
    
    Healthy <- c(Healthy, predicted[[k]]$Healthy)
    Wilted <- c(Wilted, predicted[[k]]$Wilted)
    Dead <- c(Dead, predicted[[k]]$Dead)
    
  }
  
  mean_Healthy <- app(Healthy, fun= "mean")
  mean_Wilted <- app(Wilted, fun = "mean")
  mean_Dead <- app(Dead, fun = "mean")
  
  sd_Healthy <- app(Healthy, fun= "mean")
  sd_Wilted <- app(Wilted, fun = "mean")
  sd_Dead <- app(Dead, fun = "mean")
  
}
