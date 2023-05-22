# Copyright 2023 Carlos Sáez Silvestre <carsaesi@upv.es>
# Departamento de Física Aplicada, Universitat Politècnica de València, España
#
# Lea el fichero LICENSE para los términos de licencia.

#' Calcula la frecuencia percibida por un receptor donde emisor y/o receptor se
#' encuentran en movimiento. Nota: usar el mismo número de dimensiones para
#' todos los vectores de entrada.
#'
#' @param frecuencia_emitida Frecuencia de emisión
#' @param pos_emisor Posicion del emisor
#' @param pos_receptor Posicion del receptor
#' @param v_emisor Velocidad del emisor
#' @param v_receptor Velocidad del receptor
#' @param c Velocidad de propagación de la onda, por defecto el valor de la
#'  velocidad del sonido en atmósfera terrestre a 20º, 50% humedad y a nivel
#'  del mar.
#'
#' @return Frecuencia percibida por el receptor
#'
#' @examples
#' doppler_frecuencia_percibida(frecuencia_emitida = 600, 
#' pos_emisor = c(0,0,0), pos_receptor = c(100,50,0),
#' v_emisor = c(20,0,0), v_receptor = c(0,2,0), c = 330)
#' 
#' @export
doppler_frecuencia_percibida <- function(frecuencia_emitida = 440,
                                         pos_emisor = c(0,0,0),
                                         pos_receptor = c(1,1,0),
                                         v_emisor = c(-1,-1,0),
                                         v_receptor = c(1,2,0),
                                         c = 343.2){
  
  
  norma <- function(x) sqrt(sum(x^2))
  
  r_er = pos_receptor - pos_emisor
  u_er = r_er / norma(r_er)
  
  frecuencia_percibida = frecuencia_emitida * ((c - v_receptor %*% u_er)/(c - v_emisor %*% u_er))
  
  return(as.double(frecuencia_percibida))
}