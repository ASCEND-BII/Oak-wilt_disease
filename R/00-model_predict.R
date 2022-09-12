model_predict <- function(CCI_year, CRE_year, NDW_year, KNV_year, model) {
  
  
  
  
  
  
  
  
  
  
  
  numerador <- exp(scene$dCCI * model$finalModel$coefficients[2] +
                   scene$dNDW * model$finalModel$coefficients[3] +
                   scene$dCRE * model$finalModel$coefficients[4] +
                   scene$kND * model$finalModel$coefficients[5] +
                   model$finalModel$coefficients[1])
  
  denominador <- 1 + numerador
  
  probability <- numerador/denominador
  
}
