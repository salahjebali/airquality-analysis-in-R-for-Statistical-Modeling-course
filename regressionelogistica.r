### REGRESSIONE LOGISTICA ###

data(airquality)
aria = na.omit(airquality)
data = aria[c(1,2,3,4)]

quality_treshold = 120

str(data)
#Converto le unita di misura delle variabili secondo il SI
data$Ozone <- data$Ozone * 2 # converto da ppb a mug/m3
data$Solar.R <- data$Solar.R * 41840 # converto da Ly a J/m2
data$Wind <- round(data$Wind / 2.24, 3) # converto da mph a m/s
data$Temp <- round((data$Temp - 32) / 1.8, 3) # converto da farnheit a celsius

attach(data)
#Creo una variabili dicotomizzata rispetto ai valori limiti dell'OMS
Quality <- as.numeric(data$Ozone > quality_treshold)
data$Quality <- Quality #aggiungo la variabile al mio dataset

#plots
plot(Temp, Quality)
plot(Wind, Quality)
plot(Solar.R, Quality)

#boxplots
boxplot(Temp  ~ Quality)
boxplot(Wind  ~ Quality)
boxplot(Solar.R  ~ Quality)
boxplot(Ozone ~ Quality)

### MODELLO LINEARE ###

# #Modello lineare completo
# mq <- lm (Quality ~ Wind + Temp + Solar.R)
# summary(mq)
# 
# #Modello lineare ridotto
# mq1 <- lm(Quality ~ Wind + Temp)
# summary(mq1)
# beta <- mq1$coefficients
# #stima probabilità condizionata
# pstima <- mq1$fitted.values
# summary(pstima)
# pstima

### MODELLO LOGISTICO ###

#Modello logistico - Temp
fit <- glm(Quality ~ Temp, family = binomial)

#Modello logistico - Wind
fit1 <- glm(Quality ~ Wind, family = binomial)

#Modello logistico - Solar.R 
fit2 <- glm(Quality ~ Solar.R, family = binomial)

#Modello logistico - Completo
fit3 <- glm(Quality ~ Temp + Wind + Solar.R, family = binomial)

#Modello logistico ~ Wind + Temp
fit4 <- glm(Quality ~ Temp + Wind, family = binomial)

#Modello logistico ~ Wind + Solar.R
fit5 <- glm(Quality ~ Wind + Solar.R, family = binomial)

#Modello logistico ~ Temp + Solar.R
fit6 <- glm(Quality ~ Temp + Solar.R, family = binomial)

#Pseudo R2
pseudoRfit <- ((fit$null.deviance/-2) - (fit$deviance/-2)) / ((fit$null.deviance/-2))
pseudoRfit1 <- ((fit1$null.deviance/-2) - (fit1$deviance/-2)) / ((fit1$null.deviance/-2))
pseudoRfit2 <- ((fit2$null.deviance/-2) - (fit2$deviance/-2)) / ((fit2$null.deviance/-2))
pseudoRfit3 <- ((fit3$null.deviance/-2) - (fit3$deviance/-2)) / ((fit3$null.deviance/-2))
pseudoRfit4 <- ((fit4$null.deviance/-2) - (fit4$deviance/-2)) / ((fit4$null.deviance/-2))
pseudoRfit5 <- ((fit5$null.deviance/-2) - (fit5$deviance/-2)) / ((fit5$null.deviance/-2))
pseudoRfit6 <- ((fit6$null.deviance/-2) - (fit6$deviance/-2)) / ((fit6$null.deviance/-2))

#Summaries
summary(fit)
pseudoRfit
confint(fit)

summary(fit1)
pseudoRfit1
confint(fit1)

summary(fit2)
pseudoRfit2
confint(fit2)

summary(fit3)
pseudoRfit3
confint(fit3)

summary(fit4)
pseudoRfit4
confint(fit4)

summary(fit5)
pseudoRfit5
confint(fit5)

summary(fit6)
pseudoRfit6
confint(fit6)


#TEST DEL RAPPORTO DI VEROSIMIGLIANZA
library(lmtest)
lrtest(fit4, fit3) #nested vs complex

#Lodds solo del modello prescelto
coeff <- fit4$coefficients
Lodds <- coeff[1] + coeff[2]*data$Temp + coeff[3]*data$Wind
summary(Lodds)
plot(Temp,Lodds)
abline(0, 0)
plot(Wind, Lodds)
abline(0,0)

#Plot with ggplot

library(ggeffects)
plot(ggpredict(fit4,"Temp")) # Marginal effect keeping Wind costant
plot(ggpredict(fit4,"Wind")) # Marginal effect keeping Temp constant

plot(ggpredict(fit4,c("Wind","Temp")))# Marginal effect keeping various levels of Temp
plot(ggpredict(fit4,c("Temp", "Wind")))# Marginal effect keeping various levels of Wind

#Plot with patchwork

library(patchwork)

plts = lapply(names(coefficients(fit4))[-1],function(i){
  return(plot(ggpredict(fit4,i)))
})

wrap_plots(plts)

#Stima di probabilità 

# stima <- exp(coef(fit4)%*%c(1,data$Temp[40], data$Wind
#                             [40])/(1+exp(coef(fit4)%*%c(1,data$Temp[40],data$Wind[40]))))
# stima 
# data[-2]

# pstima <- fit4$fitted.values
# summary(pstima)

detach(data)




