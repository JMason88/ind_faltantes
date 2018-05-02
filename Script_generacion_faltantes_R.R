library(modeest)

##-----------------------------------------------------------------------
##Se define función para generar Datos Faltantes de un modo completamente aleatorio --> generación de tipo MCAR en terminología de Rubin.

add_missing <- function(x, miss_prop) {
  for(i in 2:(length(x)-1)) {
    inst.aleat<-sample(1:nrow(x), nrow(x)*miss_prop, replace=F)
    x[inst.aleat, i]<-NA
  }
  return(x)}

##-----------------------------------------------------------------------
##Se define una función para imputar la Moda de cada atributo como estrategia de manipulación de datos faltantes - en caso que se presente una distribución multimodal se imputa el promedio de las modas.

replace_moda <- function(x) {
  for(i in 2:(length(x)-1)) {
    moda <- mfv(x[,i])
    x[is.na(x[,i]),i] <- mean(moda)
  } 
  return(x)}

##-----------------------------------------------------------------------
##Se define una función para imputar la Moda de cada atributo de acuerdo a la clase como estrategia de manipulación de datos faltantes - en caso que se presente una distribución multimodal se imputa el promedio de las modas.

replace_modaclase <- function(x) {
  y <- x[x[,length(x)]=="si",]
  
  for(i in 2:(length(y)-1)) {
    moday <- mfv(y[,i])
    y[is.na(y[,i]),i] <- mean(moday)
  }
  
  z <- x[x[,length(x)]=="no",]
  for(i in 2:(length(z)-1)) {
    modaz <- mfv(z[,i])
    z[is.na(z[,i]),i] <- mean(modaz)
  }
  
  x <- rbind(y,z)
  
  return(x)}

##-----------------------------------------------------------------------
##Por ultimo se compila el script completo para la inducción de faltantes. --NO FUNCIONA REVISAR--

ind_faltantes <- function(x, miss_prop, estrategia_relleno = TRUE) {
  dataset_faltantes <- add_missing(x, miss_prop)
  ifelse(estrategia_relleno, dataset_output <- replace_moda(dataset_faltantes), dataset_output <- replace_modaclase(dataset_faltantes))
  return(dataset_output)}

##-----------------------------------------------------------------------
