setwd("C:/Users/kaido/OneDrive/Escritorio/Scripts R/Análisis de regresión")
library('dplyr')
library(stringr)
library(xtable)

db <- read.csv("diabetes.csv")
missings <- function(x) return(sum(is.na(x)))
apply(db,2,missings)

#Elegimos la variable Outcome
str(db)
head(db)
summary(db)

#b) Estimación de modelos

fit1 <- glm(Outcome~Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age, family=binomial("logit"), data=db)
summary(fit1)
adj_pseudoR2<-with(fit1,1-deviance*df.null/(null.deviance*df.residual)) #pseudo R^2
a<-stepCriterion(fit1, direction="backward", criterion="aic")

fit2 <- update(fit1,formula. = a$final)
summary(fit2)

fit3<-update(fit2,family=binomial("probit"))
fit4<-update(fit2,family=binomial("cloglog"))
fit5<-update(fit2,family=binomial("cauchit"))

adj_pseudoR2<-function(fit){
  with(fit,1-deviance*df.null/(null.deviance*df.residual))
}

tabla<-cbind(rsq=cbind(c(adj_pseudoR2(fit1),adj_pseudoR2(fit2),adj_pseudoR2(fit3),adj_pseudoR2(fit4),adj_pseudoR2(fit5))),AIC(fit1,fit2,fit3,fit4,fit5),BIC(fit1,fit2,fit3,fit4,fit5))



#i) Validación de los supuestos###
envelope(fit2)
plot(fit2)

###Matriz confusion###
glm.probs <- predict(fit2, type = "response")
glm.probs[1:10]

glm.pred <- rep(0, 768)
glm.pred[glm.probs > .7] = 1

table(glm.pred, db$Outcome)
(445 + 157) / 768
mean(glm.pred == db$Outcome)


####Bonus####


table(db['Outcome']) #500 tienen Outcome 0 y 268 tienen outcome 1


#Seccionamiento de datos entrenamiento/testeo
set.seed(1)
train1<-sample(0:nrow(db),size=0.7*nrow(db),replace=FALSE)
train<-rep(FALSE,nrow(db))
train[train1] <- TRUE
test <- (!train)
test <- db[!train, ]
outcome_test<-db$Outcome[!train]
dim(test)
length(outcome_test)

summary(fit4)


#Estimación del mejor modelo con datos de entrenamiento
glm.fits <- glm(
  Outcome ~ Pregnancies+Glucose+BloodPressure+Insulin+BMI
  +DiabetesPedigreeFunction+Age,
  data = db, family = binomial, subset = train
)
#Testeo
glm.probs <- predict(glm.fits, newdata=test,
                      type = "response")

summary(glm.fits)
summary(glm.probs) #importante revisar el rango de probabilidades que tiene la respuesta
tau<-seq(0,1,by=0.1)

#Mejor Tau por habilidad predictiva
AER<-NULL
recall<-NULL
precision<-NULL
F1<-NULL
exito<-"1"
frac<-"0"

for (i in 1:length(tau)){
  glm.pred <- rep(0, 231)
  glm.pred[glm.probs > tau[i]] <- 1
  tab<-table(glm.pred,outcome_test)
  if (!frac %in% rownames(tab)){
    tab<-rbind(tab,c(0,0))
    rownames(tab)[2]<-frac
  } 
  if (!exito %in% rownames(tab)){
    tab<-rbind(tab,c(0,0))
    rownames(tab)[2]<-exito
  }
  AER[i]<-(tab[exito,frac]+tab[frac,exito])/sum(tab)
  precision[i]<-(tab[exito,exito])/sum(tab[exito,])
  recall[i]<-(tab[exito,exito])/sum(tab[,exito])
  F1[i]<-2*precision[i]*recall[i]/(precision[i]+recall[i])
}

cbind(tau,AER,precision,recall,F1)


#Tablas para overleaf
print(xtable(tabla), include.rownames = FALSE)
print(xtable(glm.fits),include.rownames=TRUE)
ROCc(cbind(outcome_test,glm.probs))

