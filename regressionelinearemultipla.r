### REGRESSIONE LINEARE MULTIPLA ###

data(airquality)
aria = na.omit(airquality)
data = aria[c(1,2,3,4)]

#CONVERSIONE DELLE VARIABILI IN SISTEMA INTERNAZIONALE DI MISURA
data$Ozone <- data$Ozone * 2 # converto da ppb a mug/m3
data$Solar.R <- data$Solar.R * 41840 # converto da Ly a J/m2
data$Wind <- round(data$Wind / 2.24, 3) # converto da mph a m/s
data$Temp <- round((data$Temp - 32) / 1.8, 3) # converto da farnheit a celsius

attach(data)

##  MODELLO DI REGRESSIONE LINEARE MULTIPLA ## 

# COMPLETO #
mq <- lm(Ozone ~ Solar.R + Wind + Temp)
summary(mq)
#plot(mq)
#RESIDUI
res <- mq$residuals
qqnorm(res)

#SIGMA
coef <- coef(mq)
sigma <- vcov(mq)

#INTERVALLI DI CONFIDENZA
confint(mq)

# t <- qt(0.975, mq$df)
# beta <- mq$coeff[c(2,3, 4)]
# std.beta <- summary(mq)$coeff[c(2,3,4),2]
# inf <- beta - t*std.beta
# sup <- beta + t*std.beta
# cbind(inf, sup)
# 
# #ELLISSOIDE DI CONFIDENZA
# library(ellipse)
# plot(ellipse(mq, c(2,3,4)), type="l")
# abline(v=c(inf[1], sup[1]), lty=2)
# abline(h=c(inf[2], sup[2]), lty=2)
# points(beta[1], beta[2],pch=18)

# RIDOTTO WIND + TEMP #
mq1 <- lm(Ozone ~ Wind + Temp)
summary(mq1)
#plot(mq1)

#RESIDUI
res1 <- mq1$residuals
qqnorm(res1)

#INTERVALLO DI CONFIDENZA
confint(mq1)

#ELLISSOIDE DI CONFIDENZA
t1 <- qt(0.975, mq1$df)
beta1 <- mq1$coeff[c(2,3)]
std.beta1 <- summary(mq1)$coeff[c(2,3),2]
inf1 <- beta1 - t1*std.beta1
sup1 <- beta1 + t1*std.beta1
cbind(inf1, sup1)

library(ellipse)
plot(ellipse(mq1, c(2,3)), type="l")
abline(v=c(inf1[1], sup1[1]), lty=2)
abline(h=c(inf1[2], sup1[2]), lty=2)
points(beta1[1], beta1[2], pch=18)

# RIDOTTO TEMP #
mq2 <- lm(Ozone ~ Temp)
summary(mq2)
#plot(mq2)
#RESIDUI
res2 <- mq2$residuals
qqnorm(res2)

#INTERVALLI DI CONFIDENZA
confint(mq2)

# RIDOTTO WIND #
mq3 <- lm(Ozone ~ Wind)
summary(mq3)
#plot(mq3)
#RESIDUI
res3 <- mq3$residuals
qqnorm(res3)

#INTERVALLI DI CONFIDENZA
confint(mq3)

# RIDOTTO SOLAR.R #
mq4 <- lm(Ozone ~ Solar.R)
summary(mq4)
#plot(mq4)
#RESIDUI
res4 <- mq4$residuals
qqnorm(res4)

#INTERVALLI DI CONFIDENZA
confint(mq4)


## TEST DEL RAPPORTO DI VEROSIMIGLIANZA ##
library(lmtest)
lrtest(mq1, mq) #nested vs complex
# 
# #Test del rapporto di verosimiglianza ricostruito analiticamente
# A <- logLik(mq1)
# B <- logLik(mq)
# teststat <- -2*(as.numeric(A) - as.numeric(B)) # df = dfB - dfA = 4 - 3 = 1
# p.val <- pchisq(teststat, df = 1, lower.tail = FALSE)
# p.val
# 
# lrtest(mq2, mq) # nested vs complex
# lrtest(mq2, mq1)  #nested vs complex

# summary(mq)
# summary(mq1)
# summary(mq2)
# summary(mq3)
# summary(mq4)

## PLOT ##
library(ggeffects)
plot(ggpredict(mq, c("Temp", "Wind", "Solar.R")))

#plot(mq)
#plot(mq1)
detach(data)















