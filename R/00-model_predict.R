model_predict <- function(scene, model) {
  
  numerador <- exp(scene$VGV * model$finalModel$coefficients[2] +
                   scene$VSS * model$finalModel$coefficients[3] +
                   scene$VPA * model$finalModel$coefficients[4] +
                   scene$IGS * model$finalModel$coefficients[5] +
                   model$finalModel$coefficients[1])
  
  denominador <- 1 + numerador
  
  probability <- numerador/denominador
  
}
