### STUDIO DI CORRELAZIONE ###

data(airquality)
aria = na.omit(airquality)
data = aria[c(1,2,3,4)]
quality_treshold = 120

#CONVERSIONE DELLE VARIABILI IN SISTEMA INTERNAZIONALE DI MISURA
data$Ozone <- data$Ozone * 2 # converto da ppb a mug/m3
data$Solar.R <- data$Solar.R * 41840 # converto da Ly a J/m2
data$Wind <- round(data$Wind / 2.24, 3) # converto da mph a m/s
data$Temp <- round((data$Temp - 32) / 1.8, 3) # converto da farnheit a celsius
Quality <- as.numeric(data$Ozone > quality_treshold)
data$Quality <- Quality #aggiungo la variabile al mio dataset

attach(data)

library(corrplot)

# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(data)
M <- cor(data)
col<- colorRampPalette(c("red", "white", "blue"))(20)
corrplot(M, method = "number", type = "upper", order = "hclust", col=col, p.mat = p.mat, sig.level = 0.01, insig = "blank")

detach(data)