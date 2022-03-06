### SELEZIONE DEL MODELLO ###

data(airquality)
aria = na.omit(airquality)
data = aria[c(1,2,3,4)]
data

quality_treshold = 120



data$Ozone <- data$Ozone * 2 # converto da ppb a mug/m3
data$Solar.R <- data$Solar.R * 41840 # converto da Ly a J/m2
data$Wind <- round(data$Wind / 2.24, 5) # converto da mph a m/s
data$Temp <- round((data$Temp - 32) / 1.8, 5) # converto da farnheit a celsius

attach(data)
#Creo una variabili dicotomizzata rispetto ai valori limiti dell'OMS
Quality <- as.numeric(data$Ozone > quality_treshold)
data$Quality <- Quality #aggiungo la variabile al mio dataset


### MODEL SELECTION ### 

# VEROSIMIGLIANZA: K = 0 -> SCEGLIE SEMPRE IL MODELLO COMPLETO
# AIC: K = 2 -> SCEGLIE IL MODELLO CHE PREDICE MEGLIO 
# BIC: K = log(length(y)) -> SCEGLI IL MODELLO PIU PARSIMONIOSO

## REGRESSIONE LINEARE MULTIPLA ## 

#Modello Completo
mq <- lm(Ozone ~ Solar.R + Wind + Temp)
#Modello Nullo
mq0 <- lm(Ozone ~ 1)

## FORWARD ##

forw_lik <- step(mq0, scope=formula(mq), direction = "forward", k=0)
forw_aic <- step(mq0, scope=formula(mq), direction = "forward", k=2)
forw_bic <- step(mq0, scope=formula(mq), direction = "forward", k=log(length(Ozone)))

## MISTO ##

both_lik <- step(mq, scope=formula(mq), direction = "both", k=0)
both_aic <- step(mq, scope=formula(mq), direction = "both", k=2)
both_bic <- step(mq, scope=formula(mq), direction = "both", k=log(length(Ozone)))

## BACKWARD ##

back_lik <- step(mq, scope=formula(mq0), direction = "backward", k=0)
back_aic <- step(mq, scope=formula(mq0), direction = "backward", k=2)
back_bic <- step(mq, scope=formula(mq0), direction = "backward", k=log(length(Ozone)))


## REGRESSIONE LOGISTICA ##

#Modello Completo
fit <- glm(Quality ~ Solar.R + Wind + Temp, family = binomial)

#Modello Nullo
fit0 <- glm(Quality ~ 1, family = binomial)

## FORWARD ## 

reg_forw_lik <- step(fit0, scope=formula(fit), direction = "forward", k=0)
reg_forw_aic <- step(fit0, scope=formula(fit), direction = "forward", k=2)
reg_forw_bic <- step(fit0, scope=formula(fit), direction = "forward", k=log(length(Quality)))

## MISTO ##

reg_both_lik <- step(fit, scope=formula(fit), direction = "both", k=0)
reg_both_aic <- step(fit, scope=formula(fit), direction = "both", k=2)
reg_both_bic <- step(fit, scope=formula(fit), direction = "both", k=log(length(Quality)))

## BACKWARD ## 

reg_back_lik <- step(fit, scope=formula(fit0), direction = "backward", k=0)
reg_back_aic <- step(fit, scope=formula(fit0), direction = "backward", k=2)
reg_back_bic <- step(fit, scope=formula(fit0), direction = "backward", k=log(length(Quality)))


detach(data)
