### REGRESSIONE POLINOMIALE MULTIPLA ###

data(airquality)
aria = na.omit(airquality)
data = aria[c(1,2,3,4)]

#CONVERSIONE DELLE VARIABILI IN SISTEMA INTERNAZIONALE DI MISURA
data$Ozone <- data$Ozone * 2 # converto da ppb a mug/m3
data$Solar.R <- data$Solar.R * 41840 # converto da Ly a J/m2
data$Wind <- round(data$Wind / 2.24, 3) # converto da mph a m/s
data$Temp <- round((data$Temp - 32) / 1.8, 3) # converto da farnheit a celsius

attach(data)

## MODELLO POLINOMIALE DI REGRESSIONE LINEARE MULTIPLA WIND^2##
d=2
pq <- lm(Ozone ~ Solar.R + poly(Wind, degree = d, raw = T) + Temp)
summary(pq)
confint(pq)
#plot(pq)
library(ggeffects)
plot(ggpredict(pq, c("Wind", "Temp", "Solar.R")))

## MODELLO POLINOMIALE DI REGRESSIONE LINEARE MULTIPLA WIND^2 + TEMP^2 ##
pq1 <- lm(Ozone ~ Solar.R + poly(Wind, degree = d, raw = T) + poly(Temp, degree = d, raw = T))
plot(ggpredict(pq1, c("Wind", "Temp", "Solar.R")))
plot(ggpredict(pq1, c("Temp", "Wind", "Solar.R")))

## MODELLO POLINOMIALE DI REGRESSIONE LINEARE MULTIPLA COMPLETO ## 
pq2 <- lm(Ozone ~ poly(Solar.R, degree = d, raw = T) + 
            poly(Wind, degree = d, raw = T) + 
            poly(Temp, degree = d, raw = T))
plot(ggpredict(pq2, c("Wind", "Temp", "Solar.R")))
plot(ggpredict(pq2, c("Solar.R", "Wind", "Temp")))
plot(ggpredict(pq2, c("Temp", "Wind", "Solar.R")))



## MODELLO DI REGRESSIONE LINEARE COMPLETO ##
mq <- lm(Ozone ~ Solar.R + Wind + Temp)
anova(mq, pq) # H0: mq, H1: pq
anova(pq,pq1) # Ho: pq, H1: pq1
anova(mq,pq1) # H0: mq, H1: pq1
anova(pq1,pq2)

detach(data)