library(corrplot)
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
library(lmtest)
library(ISLR2)
library(leaps)
library(stringr)
library(ggplot2)
library(fastDummies)
library(L1pack)
library(readxl)
library(car)
library(glmnet)
library(gam)
library(splines)


########################################Lectura y limpieza de datos #######################################
setwd("~/Documentos Ander/UNAL/Proyecto UNAL")


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
})

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

v_cuant<-cars[, lapply(cars, is.numeric) == TRUE, with = FALSE]
v_categor <- cars[, lapply(cars, is.numeric) == FALSE, with = FALSE]

#####Partición 70-30

set.seed(7)
train1<-sample(1:nrow(cars2),size=0.7*nrow(cars2),replace=FALSE)
train<-rep(FALSE,nrow(cars2))
train[train1] <- TRUE
test <- (!train)
cars.train = cars2[train, ]
cars.test = cars2[test, ]

y <- cars2$price
y.test <- y[test]
x <- model.matrix(price ~ ., cars2)[, -1]
cars.test

####Modelo Entrega 2

fit<-lm(price~1+I(carwidth)+Marca_Audi+Marca_Bmw+Marca_Buick
        +Marca_Honda+Marca_Mazda+Marca_Mitsubishi+Marca_Jaguar
        +Marca_Nissan+Marca_Peugeot+Marca_Plymouth+Marca_Porsche+Marca_Saab+Marca_Subaru
        +Marca_Dodge+Marca_Volkswagen+Marca_Volvo
        +I(enginesize^2)+Marca_Isuzu
        +carbody_hatchback+carbody_wagon+carbody_convertible+carbody_sedan
        +cylindernumber_eight+cylindernumber_four+cylindernumber_six+enginelocation_rear
        ,cars.train)
summary(fit)


entrega2.test <- predict(fit, cars.test)
mean((y.test-entrega2.test)^2)

#####Modelo del mejor subconjunto sobre la partición

system.time({regfit.full <- regsubsets(price ~ ., cars.train, really.big = TRUE)})

summary(regfit.full)$bic
####Dejamos el nvmax predeterminado porque si tomamos en cuenta todas las variables, es muy costoso computacionalmente
reg.summary <- summary(regfit.full)
which.min(reg.summary$bic)
betas<-coef(regfit.full, 9)
betas

fit.best.subset<-lm(price~1+horsepower+carwidth+curbweight+Marca_Bmw+Marca_Buick+Marca_Jaguar+Marca_Volvo+cylindernumber_eight+enginelocation_rear,data=cars.train)
best.subset.test <- predict(fit.best.subset, cars.test)
mean((y.test-best.subset.test)^2)
#####Regresión ridge ############
lambdas <- 10^seq(2, -3, length=1000)

set.seed(7)
cv_ridge <- cv.glmnet(x[train,], y[train], alpha = 0, lambda = lambdas,nfolds=10)
best_lam <- cv_ridge$lambda.min

fit.ridge <- glmnet(x, y, alpha = 0, lambda = best_lam)

ridge.test <- predict(fit.ridge, s = best_lam, newx = x[test,])
mean((y[test]-ridge.test)^2)

out2 <- glmnet(x, y, alpha = 0, lambda = lambdas)
ridge.coef <- predict(out2, type = "coefficients",
                      s = best_lam)[1:20, ]
sort(ridge.coef)

######Regresión Lasso ######################
set.seed(7)
lasso_reg <- cv.glmnet(x[train,], y[train], alpha = 1, lambda = lambdas, nfolds = 10)
lass_best_lam <- lasso_reg$lambda.min 

fit.lasso <- glmnet(x, y, alpha = 1, lambda = lass_best_lam)

lasso.test <- predict(fit.lasso, s = lass_best_lam, newx = x[test,])
mean((y[test]- lasso.test)^2)

out <- glmnet(x, y, alpha = 1, lambda = lambdas)
lasso.coef <- predict(out, type = "coefficients",
                      s = lass_best_lam)[1:52, ]
lasso.coef[abs(lasso.coef)!=0]

####    GAM      ####################



