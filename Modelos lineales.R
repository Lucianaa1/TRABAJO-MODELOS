View(airbnb_barcelona)

#ESTADÍSTICA DESCRIPTIVA
head(airbnb_barcelona)
#calcular también la correlación


#hay 16761 observaciones, es decir, 1671 airbnd de Barcelona y 14 variables explicativas, 
#que serían las características de dichos airbnb.

summary(airbnb_barcelona)

distritos<-character(length(nrow(airbnb_barcelona)))


for (i in seq_along(airbnb_barcelona$barrio)) {
  if (barrio[i] == Sant Marta) {
    distritos[i] <- "Nou Barris"
  } elseif { (barrio[i] == La Sagrada Familia) || (barrio[i]== Eixample)|| (barrio[i]== L'Antiga Esquerra de l'Eixample)|| (barrio[i]== Sant Antoni)|| (barrio[i]== Dreta de l'Eixample)|| (barrio[i]== 	
La Nova Esquerra de l'Eixample)|| (barrio[i]== el Fort Pienc)
    distritos[i] <- "L'Eixample"
  } elseif { (barrio[i]== Vila de Gracia) || (barrio[i]==
                                                Camp d'en Grassot i Gracia Nova)|| (barrio[i]==Gracia)|| (barrio[i]==Vallcarca i els Penitents)
    distritos[i] <- "Gracia"
  } elseif { (barrio[i]== Horta-Guinarda)
    distritos[i] <- "Horta"
  } elseif { (barrio[i]== Les Corts)
  distritos[i] <-"Les corts"
  } elseif { (barrio[i]== El Gotic)|| (barrio[i]== La Barceloneta)|| (barrio[i]== Ciutat Vella)|| (barrio[i]== 	
El Raval)|| (barrio[i]== Sant Pere/Santa Caterina)
  distritos[i] <- "Ciutat Vella"
  } elseif {(barrio[i]== El Poble-sec)|| (barrio[i]== Sants-Montjuic)
  distritos[i] <- "Sants-Montjuic"
  } elseif { (barrio[i]==El Clot) || (barrio[i]== El Besos i el Maresme)|| (barrio[i]== El Camp de l'Arpa del Clot)
  distritos[i]<- "Sant Martí"
   elseif {(barrio[i]==Sant Gervasi - Galvany)
  distritos[i] <-"Sarriá"}
}

  #CON AYUDA, primero hay que sacar las observaciones que no tienen el dato del barrio para que funcione el codigo
  
    distritos <- character(length(airbnb_barcelona$barrio))
  
  for (i in seq_along(airbnb_barcelona$barrio)) {
    if (airbnb_barcelona$barrio[i] == "Sant Marta") {
      distritos[i] <- "Nou Barris"
    } else if (airbnb_barcelona$barrio[i] == "La Sagrada Familia" || 
               airbnb_barcelona$barrio[i] == "Eixample" || 
               airbnb_barcelona$barrio[i] == "L'Antiga Esquerra de l'Eixample" || 
               airbnb_barcelona$barrio[i] == "Sant Antoni" || 
               airbnb_barcelona$barrio[i] == "Dreta de l'Eixample" || 
               airbnb_barcelona$barrio[i] == "La Nova Esquerra de l'Eixample" || 
               airbnb_barcelona$barrio[i] == "el Fort Pienc") {
      distritos[i] <- "L'Eixample"
    } else if (airbnb_barcelona$barrio[i] == "Vila de Gracia" || 
               airbnb_barcelona$barrio[i] == "Camp d'en Grassot i Gracia Nova" || 
               airbnb_barcelona$barrio[i] == "Gracia" || 
               airbnb_barcelona$barrio[i] == "Vallcarca i els Penitents") {
      distritos[i] <- "Gracia"
    } else if (airbnb_barcelona$barrio[i] == "Horta-Guinarda") {
      distritos[i] <- "Horta"
    } else if (airbnb_barcelona$barrio[i] == "Les Corts") {
      distritos[i] <- "Les Corts"
    } else if (airbnb_barcelona$barrio[i] == "El Gotic" || 
               airbnb_barcelona$barrio[i] == "La Barceloneta" || 
               airbnb_barcelona$barrio[i] == "Ciutat Vella" || 
               airbnb_barcelona$barrio[i] == "El Raval" || 
               airbnb_barcelona$barrio[i] == "Sant Pere/Santa Caterina") {
      distritos[i] <- "Ciutat Vella"
    } else if (airbnb_barcelona$barrio[i] == "El Poble-sec" || 
               airbnb_barcelona$barrio[i] == "Sants-Montjuic") {
      distritos[i] <- "Sants-Montjuic"
    } else if (airbnb_barcelona$barrio[i] == "El Clot" || 
               airbnb_barcelona$barrio[i] == "El Besos i el Maresme" || 
               airbnb_barcelona$barrio[i] == "El Camp de l'Arpa del Clot") {
      distritos[i] <- "Sant Martí"
    } else if (airbnb_barcelona$barrio[i] == "Sant Gervasi - Galvany") {
      distritos[i] <- "Sarriá"
    }
  }
  
  # Agregar el vector distritos al data.frame
  airbnb_barcelona$distritos <- distritos

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
mod0 <- lm(precio_euros ~  distritos + tipo_habitacion + personas + banios + habitaciones + camas + amenities + estancia_min + puntuacion, data=airbnb_barcelona)
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