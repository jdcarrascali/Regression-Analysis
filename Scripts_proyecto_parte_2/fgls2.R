fit1<-lm(price~1+I(carwidth)+I(enginesize^2)+Marca_Audi+Marca_Bmw+Marca_Buick
         +Marca_Honda+Marca_Mazda+Marca_Mitsubishi+Marca_Jaguar
         +Marca_Nissan+Marca_Peugeot+Marca_Plymouth+Marca_Porsche+Marca_Saab+Marca_Subaru
         +Marca_Dodge+Marca_Volkswagen+Marca_Volvo
         +Marca_Isuzu
         +carbody_hatchback+carbody_wagon+carbody_convertible+carbody_sedan
         +cylindernumber_eight+cylindernumber_four+cylindernumber_six+enginelocation_rear
         ,cars2)

resettest(fit1)
summary(fit1)
bptest(fit1)
vif(fit1)
plot(fit1,which=1)
plot(fit1$residuals)

modvar<-lm(log(fit1$residuals^2)~ 1+I(carwidth)+I(enginesize^2)+Marca_Audi+Marca_Bmw+Marca_Buick
           +Marca_Honda+Marca_Mazda+Marca_Mitsubishi+Marca_Jaguar
           +Marca_Nissan+Marca_Peugeot+Marca_Plymouth+Marca_Porsche+Marca_Saab+Marca_Subaru
           +Marca_Dodge+Marca_Volkswagen+Marca_Volvo
           +Marca_Isuzu+
           +carbody_hatchback+carbody_wagon+carbody_convertible+carbody_sedan
           +cylindernumber_eight+cylindernumber_four+cylindernumber_six+enginelocation_rear
           ,cars2)
summary(modvar)

fgls2 <- lm(price~1+I(carwidth)+I(enginesize^2)+Marca_Audi+Marca_Bmw+Marca_Buick
            +Marca_Honda+Marca_Mazda+Marca_Mitsubishi+Marca_Jaguar
            +Marca_Nissan+Marca_Peugeot+Marca_Plymouth+Marca_Porsche+Marca_Saab+Marca_Subaru
            +Marca_Dodge+Marca_Volkswagen+Marca_Volvo
            +Marca_Isuzu
            +carbody_hatchback+carbody_wagon+carbody_convertible+carbody_sedan
            +cylindernumber_eight+cylindernumber_four+cylindernumber_six+enginelocation_rear
            ,cars2, weights = 1/exp(modvar$fitted.values))


summary(fgls2)
plot(cars$enginesize^2,resid(fgls2)/sqrt(exp(modvar$fitted.values)))
plot(studres(fgls2),)


n<-dim(cars)[1]
X<-model.matrix(fgls2)
H<-X%*%solve(t(X)%*%X)%*%t(X)

######################### High Leverage ###############################3
n<-dim(cars)[1]
X<-model.matrix(fgls2)
H<-X%*%solve(t(X)%*%X)%*%t(X)

plot(diag(H))
abline(h=1/n,col="red")

plot(fgls2,which=5)

mean(diag(H))
4/n

plot(hatvalues(fgls2),type="h")
abline(h=2*4/n,col="red")
abline(h=3*4/n,col="blue")
sort(hatvalues(fgls2),decreasing=TRUE)

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
highleverage(fgls2)


library(MASS)

plot(fgls2,which=5)
stud_res<-studres(fgls2)
head(sort(abs(stud_res),decreasing=TRUE))
boxplot(stud_res)

corte <- 4/(n-length(fgls2$coefficients)-2) #Es una regla usada en la pr?ctica
plot(fgls2, which=4, cook.levels=corte)
abline(h=corte, lty=2, col="red")
cooksd<-cooks.distance(fgls2)
cooksd[which(cooksd>corte)]

library(car) 
influencePlot(fgls2, id.method="identify", main="Gr?fico de influencia", sub="El tama?o del c?rculo es proporcional a la D_Cook")

plot(fgls2, which=1)
plot(cars$enginesize^2,stud_res)
plot(cars$carwidth,stud_res)


###################Multicolinealidad#############################################
vif(fgls2)
eigen<-eigen(t(model.matrix(fgls2))%*%model.matrix(fgls2))
eigen$values[1]/eigen$values


##################### Patrones no explicados ######################################
plot(fgls2, which=1)
plot(fgls2$residuals^2)
plot(stud_res^2)
resettest(fgls2)


fit_var0<-lm((fgls2$residuals^2)~1+I(carwidth)+I(enginesize^2)+Marca_Audi+Marca_Bmw+Marca_Buick
             +Marca_Honda+Marca_Mazda+Marca_Mitsubishi+Marca_Jaguar
             +Marca_Nissan+Marca_Peugeot+Marca_Plymouth+Marca_Porsche+Marca_Saab+Marca_Subaru
             +Marca_Dodge+Marca_Volkswagen+Marca_Volvo
             +Marca_Isuzu
             +carbody_hatchback+carbody_wagon+carbody_convertible+carbody_sedan
             +cylindernumber_eight+cylindernumber_four+cylindernumber_six+enginelocation_rear,cars2,
             weights = 1/exp(modvar$fitted.values))
summary(fit_var0)

fit_var1<-lm((stud_res^2)~1+I(carwidth)+Marca_Audi+Marca_Bmw+Marca_Buick
             +Marca_Honda+Marca_Mazda+Marca_Mitsubishi+Marca_Jaguar
             +Marca_Nissan+Marca_Peugeot+Marca_Plymouth+Marca_Porsche+Marca_Saab+Marca_Subaru
             +Marca_Dodge+Marca_Volkswagen+Marca_Volvo
             +I(enginesize^2)+Marca_Isuzu
             +carbody_hatchback+carbody_wagon+carbody_convertible+carbody_sedan
             +cylindernumber_eight+cylindernumber_four+cylindernumber_six+enginelocation_rear,cars2,
             weights = 1/exp(modvar$fitted.values))
summary(fit_var1)

fit_var2<-lm(log(stud_res^2)~1+I(carwidth)+Marca_Audi+Marca_Bmw+Marca_Buick
             +Marca_Honda+Marca_Mazda+Marca_Mitsubishi+Marca_Jaguar
             +Marca_Nissan+Marca_Peugeot+Marca_Plymouth+Marca_Porsche+Marca_Saab+Marca_Subaru
             +Marca_Dodge+Marca_Volkswagen+Marca_Volvo
             +I(enginesize^2)+Marca_Isuzu
             +carbody_hatchback+carbody_wagon+carbody_convertible+carbody_sedan
             +cylindernumber_eight+cylindernumber_four+cylindernumber_six+enginelocation_rear,cars2,
             weights = 1/exp(modvar$fitted.values))
summary(fit_var2) #para usar FGLS si se necesita

### Alternativas cuando hay heteroscedasticidad (ver archivo heterosc.R)



########### h) Normalidad ########

#Si hay alto leverage
library(glmtoolbox) #envelope simulado
envelope(fgls2)

#para verificar si hay patrones en los residuales respecto a alguna variable
plot(sign(stud_res)[order(cars$enginesize)],type="l")


########### LAD ########

library(quantreg)
fit3<-rq(price~1+I(carwidth)+Marca_Audi+Marca_Bmw+Marca_Buick
         +Marca_Honda+Marca_Mazda+Marca_Mitsubishi+Marca_Jaguar
         +Marca_Nissan+Marca_Peugeot+Marca_Plymouth+Marca_Porsche+Marca_Saab+Marca_Subaru
         +Marca_Dodge+Marca_Volkswagen+Marca_Volvo
         +I(enginesize^2)+Marca_Isuzu
         +carbody_hatchback+carbody_wagon+carbody_convertible+carbody_sedan
         +cylindernumber_eight+cylindernumber_four+cylindernumber_six+enginelocation_rear+A
         ,tau=0.5,cars2,weights = 1/exp(modvar$fitted.values))
summary(fit3)

