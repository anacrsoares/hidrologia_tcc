# Run base script
source("C:/Users/Ana Carolina/Documents/tcc/hidrologia_tcc/ana_imerg_cleaningdata.R")

# Data Preparation
data_model <- series[1:7792, ]
Flowrate.dates <- data.frame(Flowrate.df[1:7792, 1])
Flowrate.df_sample <- Flowrate.df[1:7792,]


# Final plot
par(mfrow = c(4, 1))

data_model <- series[200:365, ]
ccf(data_model[1], data_model[2], 50, main="")
# print(ccf(data_model[1], data_model[2], 50))

# K-fold 2: 566-730
data_model <- series[566:730, ]
ccf(data_model[1], data_model[2], 50, main="")
# print(ccf(data_model[1], data_model[2], 50))

# K-fold 3: 960-1100
data_model <- series[960:1100, ]
ccf(data_model[1], data_model[2], 50, main="")
# print(ccf(data_model[1], data_model[2], 50))

# K-fold 4: 1310-1488
data_model <- series[1310:1488, ]
ccf(data_model[1], data_model[2], 50, main="")
# print(ccf(data_model[1], data_model[2], 50))


# # Data observation
# plot(Flowrate.ts[1310:1488], pch = 16, cex = 0.1, col="red",
#      xlab = "Tempo (dias)" , ylab="Vazão Observada (m³/s)")
# 
# # K-fold 1: 200-365
# plot(ts(data_model[200:365,1]), ts(data_model[200:365,2]), cex=1, xlab="Precipitação (mm)", ylab="Vazão (m³/s)")
# abline(lm(ts(data_model[,2]) ~ ts(data_model[,1]), data=data_model))
# legend("topright", legend=c("lm-regression"), lwd=2, col=c("black"))
# 
# # K-fold 2: 566-730
# plot(ts(data_model[200:365,1]), ts(data_model[200:365,2]), cex=1, xlab="Precipitação (mm)", ylab="Vazão (m³/s)")
# abline(lm(ts(data_model[,2]) ~ ts(data_model[,1]), data=data_model))
# legend("topright", legend=c("lm-regression"), lwd=2, col=c("black"))
# 
# # K-fold 3: 960-1100
# plot(ts(data_model[960:1100,1]), ts(data_model[960:1100,2]), cex=1, xlab="Precipitação (mm)", ylab="Vazão (m³/s)")
# abline(lm(ts(data_model[,2]) ~ ts(data_model[,1]), data=data_model))
# legend("topright", legend=c("lm-regression"), lwd=2, col=c("black"))
# 
# # K-fold 4: 1310-1488
# plot(ts(data_model[1310:1488,1]), ts(data_model[1310:1488,2]), cex=1, xlab="Precipitação (mm)", ylab="Vazão (m³/s)")
# abline(lm(ts(data_model[,2]) ~ ts(data_model[,1]), data=data_model))
# legend("topright", legend=c("lm-regression"), lwd=2, col=c("black"))
