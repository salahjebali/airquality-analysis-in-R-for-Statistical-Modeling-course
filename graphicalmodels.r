### GRAPHICAL MODELS ###

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
# # #Creo una variabili dicotomizzata rispetto ai valori limiti dell'OMS
Quality <- as.numeric(data$Ozone > quality_treshold)
data_quality <- data[,-1]
data_quality$Quality <- Quality #aggiungo la variabile al mio dataset
data_quality

#hist(data$Ozone)
#hist(data$Solar.R)
#hist(data$Wind)
#hist(data$Temp)
#hist(data$Quality)

library(gRain)
library(gRim)
library(gRbase)

### BACKWARD UNDIRECTIONAL GRAPH MODEL - LINEAR REGRESSION MODEL ###
sat.data <- cmod(~.^., data = data)

## AIC ##
m.data <- stepwise(sat.data)
plot(m.data)
# m.data0 <- ug(~Ozone*Solar.R + Ozone*Wind + Ozone*Temp)
# max_cliques(m.data0)
## BIC ##
m.data1 <- stepwise(sat.data, k = log(sum(data)))
plot(m.data1)

### FORWARD UNDIRECTIONAL GRAPH MODEL - LINEAR REGRESSION MODEL##
ind.data <- cmod(~.^1, data = data)

### AIC ##
m.data2 <- stepwise(ind.data, k=2, type = "unrestricted", direction = "forward", details = 0)
plot(m.data2)

m.data3 <- stepwise(ind.data, k=2, type = "decomposable", direction = "forward", details = 0)
plot(m.data3)

## BIC ##
m.data4 <- stepwise(ind.data, k = log(sum(data)), type = "unrestricted", direction = "forward", details = 0)
plot(m.data4)

m.data5 <- stepwise(ind.data, k = log(sum(data)), type = "decomposable", direction = "forward", details = 0)
plot(m.data5)

### BACKWARD UNDIRECTIONAL GRAPH MODEL - LOGISTIC REGRESSION ###
sat.data_quality <- cmod(~.^., data = data_quality)

## AIC ##
m.data_quality <- stepwise(sat.data_quality)
plot(m.data_quality)

## BIC ##
m.data_quality1 <- stepwise(sat.data_quality, k = log(sum(data_quality)))
plot(m.data_quality1)

### FORWARD UNDIRECTIONAL GRAPH MODEL - LOGISTIC REGRESSION ###
ind.data_quality <- cmod(~.^1, data = data_quality)

### AIC ##
m.data_quality2 <- stepwise(ind.data_quality, k=2, type = "unrestricted", direction = "forward", details = 0)
plot(m.data_quality2)

m.data_quality3 <- stepwise(ind.data_quality, k=2, type = "decomposable", direction = "forward", details = 0)
plot(m.data_quality3)

## BIC ##
m.data_quality4 <- stepwise(ind.data_quality, k = log(sum(data)), type = "unrestricted", direction = "forward", details = 0)
plot(m.data_quality4)

m.data_quality5 <- stepwise(ind.data_quality, k = log(sum(data)), type = "decomposable", direction = "forward", details = 0)
plot(m.data_quality5)

### DAG - LOGISTIC REGRESSION ###

library(bnlearn)
data_quality.bn <- hc(data_quality)
plot(as(amat(data_quality.bn), "graphNEL"))

### DAG - LINEAR REGRESSION ###

## BAYESIAN NAIVE DAG ##
data.bn <- hc(data)
plot(as(amat(data.bn), "graphNEL"))

## BAYESIAN PRIORITAZIED DAG ## 
block <- c(3, 1, 2, 2)
blM <- matrix(0, nrow=4, ncol=4)
rownames(blM) <- colnames(blM) <- names(data)
for (b in 2:4) blM[block==b, block<b] <- 1
library(igraph)
blackL <- data.frame(get.edgelist(as(blM, "igraph")))
names(blackL) <- c("from", "to")

data.bn1 <- hc(data, blacklist=blackL)
plot(as(amat(data.bn1), "graphNEL"))

#MORALIZED 
data.bn1.dag <- dag(~Ozone*Wind + Ozone*Temp + Ozone*Solar.R + Solar.R*Temp + Temp*Wind)
data.bn1.dag_moralized <- moralize(data.bn1.dag)
plot(data.bn1.dag_moralized)

### DAG - LOGISTIC REGRESSION ###

## BAYESIAN NAIVE DAG ##
data_quality.bn <- hc(data_quality)
plot(as(amat(data_quality.bn), "graphNEL"))

## BAYESIAN PRIORITIZED DAG ##
block <- c(1, 2, 2, 3)
blM <- matrix(0, nrow=4, ncol=4)
rownames(blM) <- colnames(blM) <- names(data_quality)
for (b in 2:4) blM[block==b, block<b] <- 1
library(igraph)
blackL <- data.frame(get.edgelist(as(blM, "igraph")))
names(blackL) <- c("from", "to")

data_quality.bn1 <- hc(data_quality, blacklist=blackL)
plot(as(amat(data_quality.bn1), "graphNEL"))

#MORALIZED
data.bn1.dag <- dag(~Ozone*Wind + Ozone*Temp + Ozone*Solar.R + Solar.R*Temp + Temp*Wind)
data.bn1.dag_moralized <- moralize(data.bn1.dag)
plot(data.bn1.dag_moralized)


### NORMALITY TEST USING SHAPIRO-WILK TEST ###

## IF P-VALUE > 0.05 WE CAN ASSUME NORMALITY
shapiro.test(data$Ozone)# NO
shapiro.test(data$Solar.R)# NO
shapiro.test(data$Wind)# SI
shapiro.test(data$Temp)# SI
shapiro.test(data_quality$Quality)# NO







