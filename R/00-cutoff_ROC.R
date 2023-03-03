# Helper function for getting the cutoff values
cutoff <- function(final) {
  
  #Rename for easy solution
  colnames(final) <- c("Healthy_true", "Symptomatic_true", "Dead_true",
                       "Healthy_pred_All", "Symptomatic_pred_All", "Dead_pred_All")
  
  healthy_pred <- prediction(final$Healthy_pred_All, final$Healthy_true)
  healthy_roc.perf = performance(healthy_pred, measure = "sens", x.measure = "spec")
  healthy_cutoff <- opt.cut(healthy_roc.perf, healthy_pred)
  
  symptomatic_pred <- prediction(final$Symptomatic_pred_All, final$Symptomatic_true)
  symptomatic_roc.perf = performance(symptomatic_pred, measure = "sens", x.measure = "spec")
  symptomatic_cutoff <- opt.cut(symptomatic_roc.perf, symptomatic_pred)
  
  dead_pred <- prediction(final$Dead_pred_All, final$Dead_true)
  dead_roc.perf = performance(dead_pred, measure = "sens", x.measure = "spec")
  dead_cutoff <- opt.cut(dead_roc.perf, dead_pred)
  
  frame_cutoff <- data.table(Condition = c("Healthy", "Symptomatic", "Dead"),
                             Specificity = c(healthy_cutoff[1], symptomatic_cutoff[1], dead_cutoff[1]),
                             Sensitivity = c(healthy_cutoff[2], symptomatic_cutoff[2], dead_cutoff[2]),
                             Cutoff = c(healthy_cutoff[3], symptomatic_cutoff[3], dead_cutoff[3]))
  
  return(frame_cutoff)
  
}

opt.cut = function(perf, pred){
  
  p <- pred@cutoffs[[1]]
  inf <- is.infinite(p)
  x <- perf@x.values[[1]]
  y <- perf@y.values[[1]]
  x <- x[!inf]
  y <- y[!inf]
  
  #plot(1-x, y)
  
  dist <- sqrt(((x - 1)^2) + ((y - 1)^2))
  ind <- which.min(dist)[1]
  
  #points(x[ind], y[ind], col = "red")
  
  return(c(specificity = (1-x[ind]),
           sensitivity = y[ind],
           cutoff = p[ind]))
}

