
mod0<-lm(score ~ gatos...2+gatos...3+ratones+pajaros+insectos, data = diagnostico)
coef(mod0)
install.packages("car")

#hacer todo de diapo 4 del tercer antes de empezar a corregir
#1
library(car)
vif(mod0)
#si el VIF es alto la variable es comb lineal de las demás, gatos2, ratones tienen VIF altos (>5)

#2
install.packages("ggplot2")
library(ggplot2)
res<-rstudent(mod0) #residuos
yhat<-fitted(mod0) #ygorro
#grafico
diagnostico$predichos<-yhat #agrego los residuos a la tabla
diagnostico$residuos<-res #agrego los ygorro a la tabla
ggplot(diagnostico, aes(x = predichos , y = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0)
#3
install.packages("skedastic")
library(skedastic)
breusch_pagan(mod0)
#hipotesis nula es que todos tienen la misma varianza, es decir, hay homoscedasticidad, la otra hipotesis es que hay al menos uno que tiene una varianza diferente a sigma cuadrado
#rechazo H0, NO HAY HOMOSEDASTICIDAD, p valor muy chico.

#4
# histogramas
hist(rstudent(mod0)) #OJO es sensible al tamaño de las barras (bins) por lo que se puede interpretar de diferentes formas
#Q-Q plot
qqPlot(res)
plot(density(diagnostico$residuos))
shapiro.test(resiudos) #mirar
#6
crPlot(mod0,"gatos...2")
crPlot(mod0,"gatos...3")
crPlot(mod0,"ratones")
crPlot(mod0,"pajaros")
crPlot(mod0,"insectos")
#7
# medidas de influencia
h_i <- influence(mod0)$hat
D_i <- cooks.distance(mod0)
df <- data.frame(i = 1:nrow(diagnostico),
                 h_i = h_i,
                 D_i = D_i)

# leverage
ggplot(df, aes(x = i, y = h_i)) +
  geom_point() +
  geom_segment(aes(x = i, xend = i, y = 0, yend = h_i)) +
  xlab('') +
  ylab(expression(h[i])) +
  geom_abline(slope = 0, intercept = 2*5/50, col = 2, linetype = 'dashed')

# Distancia de Cook
ggplot(df, aes(x = i, y = D_i)) +
  geom_point() +
  geom_segment(aes(x = i, xend = i, y = 0, yend = D_i)) +
  xlab('') +
  ylab(expression(D[i])) +
  geom_abline(slope = 0, intercept = 4/50, col = 2, linetype = 'dashed')
#si hay observaciones influyentes, en el grafico de leverage no se ve, solo se ve ruido, pero en el grafico de cook los que están por arriba de la linea roja son influyentes.

#corregir

#primero arreglo por el VIF

mod1<-update(mod0,~.-ratones)
vif(mod1)
#se arregló el vif
#volvemos a hacer el grafico
res<-rstudent(mod1) #residuos
yhat<-fitted(mod1) #ygorro
#grafico
diagnostico$predichos<-yhat #agrego los residuos a la tabla
diagnostico$residuos<-res #agrego los ygorro a la tabla
ggplot(diagnostico, aes(x = predichos , y = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0)
#el grafico cambió, en el podemos ver la homosedasticidad
breusch_pagan(mod1)
#el p_valor bajó, se sigue rechazando H0
#intervenir las observaciones atipicas(mejor que eliminarlas)
#miramos en el grafico de cook en que posición está la observación atipica
#vamos a agregar dummies, tampoco hay que pasarse.
diagnostico$I47<-0
diagnostico$I47[47]<-1
mod2<-update(mod1,~.+I47)
breusch_pagan(mod0)
#vuelvo a hacer el grafico de cook
h_i <- influence(mod2)$hat
D_i <- cooks.distance(mod2)
df <- data.frame(i = 1:nrow(diagnostico),
                 h_i = h_i,
                 D_i = D_i)
ggplot(df, aes(x = i, y = D_i)) +
  geom_point() +
  geom_segment(aes(x = i, xend = i, y = 0, yend = D_i)) +
  xlab('') +
  ylab(expression(D[i])) +
  geom_abline(slope = 0, intercept = 4/50, col = 2, linetype = 'dashed')
#seguir sacando dummies
