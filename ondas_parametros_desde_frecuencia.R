# Copyright 2023 Carlos Sáez Silvestre <carsaesi@upv.es>
# Departamento de Física Aplicada, Universitat Politècnica de València, España
#
# Lea el fichero LICENSE para los términos de licencia.

#' Calcula parámetros de onda a partir de la frecuencia y
#'  velocidad de propagación
#'
#' @param f Frecuencia de emisión
#' @param c Velocidad de propagación de la onda, por defecto el valor de la
#'  velocidad del sonido en atmósfera terrestre a 20º, 50% humedad y a nivel
#'  del mar.
#'
#' @return data.frame con los parámetros: periodo (t), logitud de onda (l),
#'  número de onda lineal (kl), frecuencia angular o pulsación (w), número
#'  de onda angular (ka).
#'
#' @examples
#' ondas_parametros_desde_frecuencia(c(0.5,1,2))
#' 
#' @export
ondas_parametros_desde_frecuencia <- function(f, c = 343.2){
  
  t = 1/f
  l = 1/t
  kl = 1/l
  w = 2*pi*f
  ka = 2*pi*kl

  resultados <- data.frame(t,l,kl,w,ka)
  names(resultados) <- c('t','l','kl','w','ka')
  
  return(resultados)
}