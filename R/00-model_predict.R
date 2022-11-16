model_predict <- function(scene, models, threads = 4) {
  
  predicted <- list()
  
  for(k in 1:length(models)) {
    
    repetition <- predict(scene, model = models[[k]], 
                          type = "prob", cores = threads, 
                          na.rm = TRUE, cpkgs = c("caret"))
    
    predicted[[i]] <- repetition
    names(predicted)[[i]] <- names(models)[i]
    
  }
  
  #Stack
  for(k in 1:length(models)) {
    
    if(k == 1) {
      
      Healthy <- predicted[[i]]$Healthy
      Wilted <- predicted[[i]]$Wilted
      Dead <- predicted[[i]]$Dead
      
    }
    
    Healthy <- c(Healthy, predicted[[i]]$Healthy)
    Wilted <- c(Wilted, predicted[[i]]$Wilted)
    Dead <- c(Dead, predicted[[i]]$Dead)
    
  }
  
  mean_Healthy <- app(Healthy, fun= "mean")
  mean_Wilted <- app(Wilted, fun = "mean")
  mean_Dead <- app(Dead, fun = "mean")
  
  sd_Healthy <- app(Healthy, fun= "mean")
  sd_Wilted <- app(Wilted, fun = "mean")
  sd_Dead <- app(Dead, fun = "mean")
  
}
