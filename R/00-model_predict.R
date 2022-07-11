model_predict <- function(scene, model) {
  
  numerador <- exp(scene$dCCI * model$finalModel$coefficients[2] +
                   scene$dNDW * model$finalModel$coefficients[3] +
                   scene$dCRE * model$finalModel$coefficients[4] +
                   scene$kND * model$finalModel$coefficients[5] +
                   model$finalModel$coefficients[1])
  
  denominador <- 1 + numerador
  
  probability <- numerador/denominador
  
}
