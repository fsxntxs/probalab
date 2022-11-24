library(dplyr)
library(rio)
X <- matrix(c(1:8,50:42,15:9,30:41,16:29), ncol=10)
sd(X[2,])

sum(sqrt(1:10))

pbinom(q=3,size=10,prob=0.3)


# EJERCICIOS  -------------------------------------------------------------

data<-import(file.choose())
tail(data)

data<-rename(data, calefaccion = cheating)
head(data)

data_con_calefaccion<-filter(data,calefaccion == 1)
head(data_con_calefaccion)

mean(data_con_calefaccion$rent)

data_alta_localizacion<-filter(data,location==3)
sd(data_alta_localizacion$rentsqm)

datita<-filter(data,calefaccion==0,location==2 | location ==1)
median(datita$area)

nrow(filter(data,kitchen == 0,bath == 0,location == 2|location == 3))

nrow(filter(data,kitchen==1,calefaccion==1))*100/nrow(data)


vectores <- c(1:4, 28:24, 5:9, 23:18, 10:17)
X <- matrix(vectores, ncol=4)
mean(X[,1])


pnorm(28,mean=29.1,sd=2.2)
library(rio)
library(dplyr)
library(tidyr)
data <- import(file.choose())
head(data)
summary(data)
###################################
distinct(select(data,centro))  ####
###################################
median(filter(data,centro=="Coquimbo")$anillos)

ddata1<- filter(data,centro=='Chiloé'| centro=='Puerto Montt')
ddata2<- filter(data,centro== c('Chiloé','Puerto Montt'))

head(ddata2)

datita<- unite(data = data, col = 'largo-diametro',largo:diametro,sep = '-');datita
datita<- separate(data = datita,col = 'largo-diametro',into = c('largo','diametro'),sep = '-');datita
sd(filter(data,pesot>350)$largo)
datita
nrow(filter(data,pesot<300,largo<=11))

nrow(filter(data,anillos>10,centro == 'Caldera'| centro == 'Chiloé' ))*100/nrow(data)

summarise(data,promedio_largo = mean(largo), promedio_peso_total = mean(pesot))

install.packages("tidyr")
install.packages("TeachingDemos")
install.packages("ggplot2")


# Ensayo 2 ----------------------------------------------------------------


data<- import(file.choose())

head(data)

X <- data$FUMA
Y <- data$NEDU

tabla <- table(X,Y);tabla

tabla_proba <- prop.table(tabla);tabla_proba
#Indique cuál es la probabilidad de que una persona 
#fume y tenga más de 12 años de estudio:
tabla_proba['Si','> 12 años']


#Indique la probabilidad de que una persona tenga como máximo 12 años 
#de estudio y que no fume:
#prob entre 8 y 12 + prob 8

sum(tabla_proba['No',c('8 - 12 años','< 8 años')])

tabla_proba

rowSums(tabla_proba)

#Indique la probabilidad de que una persona 
#fume dado que tiene más de 12 años de estudio:

X.Y <- prop.table(tabla,margin = 2); X.Y

X.Y['Si','> 12 años']


#Indique la covarianza entre las variables COLESTEROL e IMC:

cov(data$COLESTEROL,data$IMC)

cor(data$PAS,data$EDAD)

#Ajuste una distribución Normal a la variable COLESTEROL 
#por medio de un gráfico de probabilidad. ¿Qué valor 
#estimado para la media μ se obtuvo?


N <- nrow(data)

p <- (1:N)/(N+1)

y <- sort(data$COLESTEROL)

m <- lm(y ~  qnorm(p))
m$coefficients[1]

#Del ajuste de la pregunta anterior ¿Qué valor se 
#encontró para el parámetro σ?

m$coefficients[2]

#Ajuste una distribución Log-Normal a la misma variable 
#COLESTEROL por medio de un gráfico de probabilidad. 
#¿Qué valor se obtuvo para el parámetro λ?


modelo = lm(log(y)~qnorm(p))
modelo$coefficients[1]

modelo$coefficients[2]

modelo = lm(log(y) ~ qlogis(p))
modelo$coefficients[1]

modelo$coefficients[2]




