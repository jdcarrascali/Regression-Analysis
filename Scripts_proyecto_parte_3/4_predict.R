setwd("C:/Users/kaido/OneDrive/Escritorio/Scripts R/Análisis de regresión")
library('dplyr')
library(stringr)
library(xtable)
library(readr)


cars <- read_csv("CarPrice_Assignment.csv")
missings <- function(x) return(sum(is.na(x)))
apply(cars,2,missings)
cars$symboling <- as.character(cars$symboling)

marcas <- with(cars,str_split_fixed(CarName," ",2))

cars <- within(cars, {
  Marca <- str_to_title(str_squish(str_to_lower(marcas[,1])))
  Marca <-str_replace_all(Marca,"Porcshce","Porsche")
  Marca <-str_replace_all(Marca,"Toyouta","Toyota")
  Marca <-str_replace_all(Marca,"Maxda","Mazda")
  Marca <-str_replace_all(Marca,"Vokswagen","Volkswagen")
  Marca <-str_replace_all(Marca,"Vw","Volkswagen")
  Marca <-str_replace_all(Marca,"Alfa-Romero","AlfaRomeo")
  rm(CarName)
  rm(car_ID)
  #price<-log(price)
})

#Best subset

library(leaps)
system.time({
regfit.full <- regsubsets(price~ ., cars, really.big=T)
})
reg.summary <- summary(regfit.full)
names(reg.summary)
which.min(reg.summary$cp)
which.min(reg.summary$bic)
reg.summary$rsq
reg.summary$cp
reg.summary$bic
summary(regfit.full)




#Forward
system.time({
  regfit.full <- regsubsets(price~ ., cars, really.big=T,method="forward",nvmax=63)
})
reg.summary <- summary(regfit.full)
which.min(reg.summary$bic)
names(reg.summary)
reg.summary$rsq[22]
reg.summary$cp[22]
reg.summary$obj[22]
which.max(reg.summary$rsq)

#Coeficientes mejor modelo con 22 variables
coef<-coef(regfit.full, 22)
print(xtable(coef), include.rownames = FALSE)



#Backward
system.time({
  regfit.full <- regsubsets(price~ ., cars, really.big=T,method="backward",nvmax=63)
})
reg.summary <- summary(regfit.full)
which.min(reg.summary$bic)
names(reg.summary)
reg.summary$rsq[21]
reg.summary$cp[21]
reg.summary$bic[21]
which.max(reg.summary$rsq)



#Modelo entrega 2
BIC(fit)
AIC(fit)

##Extra####

par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Número de variables",
     ylab = "SCE", type = "l")
plot(reg.summary$adjr2, xlab = "Número de variables",
     ylab = " R2 Ajustado", type = "l")
###
which.max(reg.summary$adjr2)
points(9, reg.summary$adjr2[9], col = "red", cex = 2, 
       pch = 20)
###
plot(reg.summary$cp, xlab = "Número de variables",
     ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(9, reg.summary$cp[9], col = "red", cex = 2,
       pch = 20)
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Número de variables",
     ylab = "BIC", type = "l")
points(14, reg.summary$bic[14], col = "red", cex = 2,
       pch = 20)
