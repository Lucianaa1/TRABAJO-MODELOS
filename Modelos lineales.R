View(airbnb_barcelona)

#ESTADÍSTICA DESCRIPTIVA
head(airbnb_barcelona)
#calcular también la correlación


#hay 16761 observaciones, es decir, 1671 airbnd de Barcelona y 14 variables explicativas, 
#que serían las características de dichos airbnb.

summary(airbnb_barcelona)

#miramos si hay valores de las variables explicativas para todos los id

airbnb_barcelona %>% filter(is.na(puntuacion)) %>% select(puntuacion) #3891 obs sin valor 
airbnb_barcelona %>% filter(is.na(barrio)) %>% select(barrio) #1 sin valor
airbnb_barcelona %>% filter(is.na(personas)) %>% select(personas) #info de todas
airbnb_barcelona %>% filter(is.na(banios)) %>% select(banios) #9 sin valor
airbnb_barcelona %>% filter(is.na(habitaciones)) %>% select(habitaciones) #3 sin valor
airbnb_barcelona %>% filter(is.na(camas)) %>% select(camas) #16 sin valor
airbnb_barcelona %>% filter(is.na(precio_euros)) %>% select(precio_euros) #info de todas
airbnb_barcelona %>% filter(is.na(estancia_min)) %>% select(estancia_min) #info de todas
airbnb_barcelona %>% filter(is.na(amenities)) %>% select(amenities) #info de todas
airbnb_barcelona %>% filter(is.na(tipo_habitacion)) %>% select(tipo_habitacion) #info de todas
airbnb_barcelona %>% filter(is.na(longitud)) %>% select(longitud) #info de todas
airbnb_barcelona %>% filter(is.na(latitud)) %>% select(latitud) #info de todas
airbnb_barcelona %>% filter(is.na(cod_postal)) %>% select(cod_postal) #506 sin valor


#SELECCIÓN DE VARIABLES

k=ncol(airbnb_barcelona)-1
modelos_posibles=2**k-1

#hay 16383 modelos posibles, vamos a aplicar los procedimientos de hipotesis para
#llegar al mejor modelo
library(tidyverse)

#SI HAGO mod0 SE ROMPE R)??
mod0 <- lm(id ~ host_id + barrio + cod_postal + latitud + longitud + tipo_habitacion + personas + banios + habitaciones + camas + amenities + precio_euros + estancia_min + puntuacion, data=airbnb_barcelona)
coef(mod0)
summary(mod0)

#CUAL ES LA LIBRERIA??
#forward
modF <- forward(mod0, alpha = 0.05)
length(coef(modF))  # parametros
summary(modF)

# backward
modB <- backward(mod0, alpha = 0.01)
length(coef(modB))  # parametros
summary(modB)

# stepwise
modS <- stepWise(mod0, alpha.enter = 0.04, alpha.remove=0.05)
length(coef(modS))  # parametros
summary(modS)

#COMPARAMOS POR AIC, BIC O R2 AJUSTADO PARA VER CUAL ES EL MEJOR

#que paquete se usaba para ver la tabla con AIC BIC
install.packages("HH")
library(HH)
summaryHH(modF)
summaryHH(modB)

help(anova)
summaryHH(modS)