# Helper function for getting the cutoff values
cutoff <- function(final) {
  
  #Rename for easy solution
  colnames(final) <- c("Dead_true", "Healthy_true", "Symptomatic_true",
                       "Dead_pred_All", "Healthy_pred_All", "Symptomatic_pred_All")
  
  healthy_pred <- prediction(final$Healthy_pred_All, final$Healthy_true)
  healthy_roc.perf = performance(healthy_pred, measure = "tpr", x.measure = "fpr")
  healthy_cutoff <- opt.cut(healthy_roc.perf, healthy_pred)
  
  symptomatic_pred <- prediction(final$Symptomatic_pred_All, final$Symptomatic_true)
  symptomatic_roc.perf = performance(symptomatic_pred, measure = "tpr", x.measure = "fpr")
  symptomatic_cutoff <- opt.cut(symptomatic_roc.perf, symptomatic_pred)
  
  dead_pred <- prediction(final$Dead_pred_All, final$Dead_true)
  dead_roc.perf = performance(dead_pred, measure = "tpr", x.measure = "fpr")
  dead_cutoff <- opt.cut(dead_roc.perf, dead_pred)
  
  frame_cutoff <- data.table(Condition = c("Healthy", "Symptomatic", "Dead"),
                             Sensitivity = c(healthy_cutoff[1,1], symptomatic_cutoff[1,1], dead_cutoff[1,1]),
                             Specificity = c(healthy_cutoff[2,1], symptomatic_cutoff[2,1], dead_cutoff[2,1]),
                             Cutoff = c(healthy_cutoff[3,1], symptomatic_cutoff[3,1], dead_cutoff[3,1]))
  
  return(frame_cutoff)
  
}

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))[1]
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
