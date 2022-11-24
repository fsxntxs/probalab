library(rio)
library(dplyr)

data <- import(file.choose())

nueva_data <- mutate(data,n_anillos = ifelse(anillos>=3 & anillos<=7,'3-7',
                                             ifelse(anillos>=8 & anillos<=12,
                                                  '8-12',ifelse(anillos>=13 & anillos<=17,'13-17','18-23'))))
head(nueva_data)

nrow(filter(nueva_data,n_anillos == '13-17'))/nrow(data)


# Pregunta 2 --------------------------------------------------------------

X <- nueva_data$centro
Y <- nueva_data$n_anillos
tablita <- table(X,Y)
sum(prop.table(tablita)['Chiloé',c('8-12','13-17')])

prop.table(tablita)

# pregunta 3 --------------------------------------------------------------

#sea de puerto mont dado que tiene entre 3-7 anillos
X.Y <- prop.table(tablita,margin = 2)

X.Y['Puerto Montt','3-7']

# Pregunta 4 --------------------------------------------------------------

Y <- sort(data$largo)
N <- nrow(data)
p <- (1:N)/(N+1)

#Log-Normal

modelo <- lm(log(Y)~qnorm(p))

lambda <- modelo$coefficients[1]
dzeta <- modelo$coefficients[2]

lambda;dzeta


# Pregunta 5 --------------------------------------------------------------


library(fitdistrplus)

EM <- fitdist(Y,method = 'mme', distr = 'gamma')
EM$estimate

k <- EM$estimate[1]
v <- EM$estimate[2]
k;v



# Pregunta 6 --------------------------------------------------------------

EMV <- fitdist(Y, 'weibull', 'mle')

B <- EMV$estimate[1]
n <- EMV$estimate[2]
B;n


# Pregunta 7 --------------------------------------------------------------

#Porporcion base de datos
Proporcion<- nrow(filter(data,largo>=5,largo<=12))/nrow(data)
#Log-Normal(5<x<12) = LogNormal(x<12) - LogNormal(x<5)
LogNormal<- plnorm(12,meanlog = lambda, sdlog = dzeta)-plnorm(5,
                                                  meanlog = lambda,
                                                  sdlog = dzeta)
#Gamma(5<x<12) = Gamma(x<12) - Gamma(x<5)
Gamma<- pgamma(12,shape = k, rate = v)-pgamma(5,shape = k,rate=v)
#Weibull(5<x<12) = Weibull(x<12) - Weibull(x<5)
Weibull<- pweibull(12,shape=B,scale=n)-pweibull(5,shape=B,scale=n)

probabilidad <- c(Proporcion,LogNormal,Gamma,Weibull)
metodo <- c('Proporcion','LogNormal','Gamma','Weibull')
tablita <- data.frame(metodo,probabilidad);tablita


# Pregunta 8 --------------------------------------------------------------






