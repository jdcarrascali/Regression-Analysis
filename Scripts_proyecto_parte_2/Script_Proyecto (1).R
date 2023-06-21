library(psych)
library(sqldf)
library(car)
library(boot)
library(MASS)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(epiDisplay)
library(ppcor)
library(XICOR)
library(lmtest)
library(ISLR2)
library(leaps)
library(stringr)
library(ggplot2)
library(fastDummies)
library(ggpubr)

########################################Lectura y limpieza de datos #######################################
setwd('E:/Users/usuario/Documents/Regresión')

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
  rm(doornumber)
  rm(fuelsystem)
  rm(compressionratio)
  rm(stroke)
  rm(boreratio)
  #price<-log(price)
})

v_cuant<-cars[, lapply(cars, is.numeric) == TRUE, with = FALSE]
v_categor <- cars[, lapply(cars, is.numeric) == FALSE, with = FALSE]

pairs(price~.,v_cuant)
summary(v_cuant)



###################################Anális Exploratorio###############################################

cor(v_cuant)
pcor(v_cuant)[1]

modelo2<-lm(price~1+carbody+enginesize+curbweight+Marca+carwidth+Marca,cars,weights=curbweight^(-3))
plot(modelo2$residuals,x=cars$enginesize)
summary(modelo2)
resettest(modelo2)
vif(modelo2)

#Acá van las Gráficas

#####################################################################################################
################################################################################################################

cars2<-cars%>%
  select(price,enginesize,wheelbase,horsepower,Marca,carbody,carwidth,enginetype,cylindernumber,aspiration,drivewheel,curbweight
         ,enginelocation,highwaympg,peakrpm,carlength)

cars2<-dummy_cols(cars2,c("Marca","carbody","cylindernumber",
                         "aspiration","enginetype","drivewheel","enginelocation"),remove_selected_columns = TRUE)

cars2<-within(cars2,{
  rm(Marca_Toyota)
  rm(carbody_hardtop)
  rm(cylindernumber_five)
  rm(drivewheel_4wd)
  rm(enginelocation_front)
})

fit<-lm(price~1+I(carwidth)+Marca_Audi+Marca_Bmw+Marca_Buick
        +Marca_Honda+Marca_Mazda+Marca_Mitsubishi+Marca_Jaguar
        +Marca_Nissan+Marca_Peugeot+Marca_Plymouth+Marca_Porsche+Marca_Saab+Marca_Subaru
        +Marca_Dodge+Marca_Volkswagen+Marca_Volvo
        +I(enginesize^2)+Marca_Isuzu
        +carbody_hatchback+carbody_wagon+carbody_convertible+carbody_sedan
        +cylindernumber_eight+cylindernumber_four+cylindernumber_six+enginelocation_rear
        ,cars2)
summary(fit)





######################### High Leverage ###############################3
n<-dim(cars)[1]
X<-model.matrix(fit)
H<-X%*%solve(t(X)%*%X)%*%t(X)

plot(diag(H))
abline(h=1/n,col="red")

plot(fit,which=5)

mean(diag(H))
4/n

plot(hatvalues(fit),type="h")
abline(h=2*4/n,col="red")
abline(h=3*4/n,col="blue")
sort(hatvalues(fit),decreasing=TRUE)

X[c(49,50,126,129,130),]
          

highleverage <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  ratio <-p/n
  plot(hatvalues(fit), main='Index Plot of Ratio')
  abline(h=c(2,3)*ratio, col='red', lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
x11()
highleverage(fit)


library(MASS)

plot(fit,which=5)
stud_res<-studres(fit)
head(sort(abs(stud_res),decreasing=TRUE))
boxplot(stud_res)

corte <- 4/(n-length(fit$coefficients)-2) #Es una regla usada en la pr?ctica
plot(fit, which=4, cook.levels=corte)
abline(h=corte, lty=2, col="red")
cooksd<-cooks.distance(fit)
cooksd[which(cooksd>corte)]

library(car) 
influencePlot(fit, id.method="identify", main="Gr?fico de influencia", sub="El tama?o del c?rculo es proporcional a la D_Cook")

plot(fit, which=1)
plot(cars$enginesize^2,stud_res)
plot(cars$carwidth,stud_res)


###################Multicolinealidad#############################################
vif(fit)
eigen<-eigen(t(model.matrix(fit))%*%model.matrix(fit))
eigen$values[1]/eigen$values


##################### Patrones no explicados ######################################
plot(fit, which=3)
plot(fit$residuals^2)
plot(stud_res^2)


fit_var0<-lm((fit$residuals^2)~1+I(carwidth)+Marca_Audi+Marca_Bmw+Marca_Buick
             +Marca_Honda+Marca_Mazda+Marca_Mitsubishi+Marca_Jaguar
             +Marca_Nissan+Marca_Peugeot+Marca_Plymouth+Marca_Porsche+Marca_Saab+Marca_Subaru
             +Marca_Dodge+Marca_Volkswagen+Marca_Volvo
             +I(enginesize^2)+Marca_Isuzu
             +carbody_hatchback+carbody_wagon+carbody_convertible+carbody_sedan
             +cylindernumber_eight+cylindernumber_four+cylindernumber_six+enginelocation_rear,cars2)
summary(fit_var0)

fit_var1<-lm((stud_res^2)~1+I(carwidth)+Marca_Audi+Marca_Bmw+Marca_Buick
             +Marca_Honda+Marca_Mazda+Marca_Mitsubishi+Marca_Jaguar
             +Marca_Nissan+Marca_Peugeot+Marca_Plymouth+Marca_Porsche+Marca_Saab+Marca_Subaru
             +Marca_Dodge+Marca_Volkswagen+Marca_Volvo
             +I(enginesize^2)+Marca_Isuzu
             +carbody_hatchback+carbody_wagon+carbody_convertible+carbody_sedan
             +cylindernumber_eight+cylindernumber_four+cylindernumber_six+enginelocation_rear,cars2)
summary(fit_var1)

fit_var2<-lm(log(stud_res^2)~1+I(carwidth)+Marca_Audi+Marca_Bmw+Marca_Buick
             +Marca_Honda+Marca_Mazda+Marca_Mitsubishi+Marca_Jaguar
             +Marca_Nissan+Marca_Peugeot+Marca_Plymouth+Marca_Porsche+Marca_Saab+Marca_Subaru
             +Marca_Dodge+Marca_Volkswagen+Marca_Volvo
             +I(enginesize^2)+Marca_Isuzu
             +carbody_hatchback+carbody_wagon+carbody_convertible+carbody_sedan
             +cylindernumber_eight+cylindernumber_four+cylindernumber_six+enginelocation_rear,cars2)
summary(fit_var2) #para usar FGLS si se necesita

### Alternativas cuando hay heteroscedasticidad (ver archivo heterosc.R)



########### h) Normalidad ########

#Si hay alto leverage
library(glmtoolbox) #envelope simulado
envelope(fit)

#para verificar si hay patrones en los residuales respecto a alguna variable
plot(sign(stud_res)[order(cars$enginesize)],type="l")


########### LAD ########

library(quantreg)
fit2<-rq(price~1+I(carwidth)+Marca_Audi+Marca_Bmw+Marca_Buick
         +Marca_Honda+Marca_Mazda+Marca_Mitsubishi+Marca_Jaguar
         +Marca_Nissan+Marca_Peugeot+Marca_Plymouth+Marca_Porsche+Marca_Saab+Marca_Subaru
         +Marca_Dodge+Marca_Volkswagen+Marca_Volvo
         +I(enginesize^2)+Marca_Isuzu
         +carbody_hatchback+carbody_wagon+carbody_convertible+carbody_sedan
         +cylindernumber_eight+cylindernumber_four+cylindernumber_six+enginelocation_rear
         ,tau=0.5,cars2)
summary(fit2)



