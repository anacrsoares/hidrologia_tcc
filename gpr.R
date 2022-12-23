# Loading libraries
library(kernlab)  # gausspr function
library(miscTools)  # R2 function
library(Metrics)  # MAE
library(ggplot2)
library(MLmetrics)

# Run base script
source("C:/Users/Ana Carolina/Documents/tcc/hidrologia_tcc/ana_imerg_cleaningdata.R")

# Getting data
data_model <- series[1:7792, ]

Flowrate.dates <- data.frame(Flowrate.df[1:7788, 1])
Flowrate.df_sample <- Flowrate.df[1:7788,]

# Lagging precipitation
data_model <- transform(data_model, lag_1=c(Precipitation.ts[-1], NA))
data_model <- transform(data_model, lag_2=c(lag_1[-1], NA))
data_model <- transform(data_model, lag_3=c(lag_2[-1], NA))
data_model <- transform(data_model, lag_4=c(lag_3[-1], NA))
data_model <- data_model[1:7788,]

# Data splitting
train.gp <- data_model[1:6000, c(2, 4:6)]
test.gp <- data_model[6001:7788, c(2, 4:6)]

# Gaussian Processes
set.seed(1)
model.gp <- gausspr(Flowrate.ts ~ ., data=train.gp, type="regression", 
                    kernel = "rbfdot", var=0.05,
                    na.omit=TRUE)
print(model.gp)

####################  Prediction – train data ####################
set.seed(1)
pred_train.gp <- predict(model.gp, train.gp)
flowrate_train_predicted <- ts(pred_train.gp)

# Metrics
abs_diff_train <- abs(flowrate_train_predicted - train.gp$Flowrate.ts)

MAE <- mean(abs_diff_train, na.rm = TRUE)
RMSE <- sqrt(mean((flowrate_train_predicted-train.gp$Flowrate.ts)^2 , na.rm = TRUE))
MSE <- RMSE^2
MAPE <- mape(flowrate_train_predicted, train.gp$Flowrate.ts)
R2 <- rSquared(train.gp$Flowrate.ts, resid = abs_diff_train)

cat(" MAE:", MAE, "\n", "RMSE:", RMSE, "\n", "MSE:", MSE, "\n", "MAPE:", MAPE, "\n", "R-squared:", R2)

# # Plots
# ## Plot 1
# par(mfrow = c(1, 1))
# plot(pred_train.gp ~ train.gp$Flowrate.ts, pch = 16, cex = 0.1, 
#      xlab = "Observações (m³/s)" , ylab=expression(Predições (m^3/s)))
# reg_train.gp <- lm(pred_train.gp ~ train.gp$Flowrate.ts)
# abline(reg_train.gp, col="red")

# ## Plot 2
# par(mfrow = c(2, 1))
# 
# plot(ts(train.gp$Flowrate.ts), pch = 16, cex = 0.1,
#      xlab = "Tempo (dias)" , ylab="Vazão Observada (m³/s)")
# 
# plot(flowrate_train_predicted, pch = 16, cex = 0.1, col="blue3",
#      xlab = "Tempo (dias)" , ylab="Vazão Prevista (m³/s)")

## Plot 3
par(mfrow = c(1, 1))
plot(Flowrate.df_sample[1:6000, 2] ~ Flowrate.df_sample[1:6000, 1],
     pch = 16, cex = 0.1, col="red",
     xlab = "Tempo (dias)" , ylab=expression(Predições (m^3/s)))
lines(Flowrate.df_sample[1:6000, 1], flowrate_train_predicted, lwd="1", col="blue")

#################### Prediction – test data ####################
pred_test.gp <- predict(model.gp, test.gp)

flowrate_test_predicted <- ts(pred_test.gp)

# Metrics
abs_diff_test <- abs(flowrate_test_predicted - test.gp$Flowrate.ts)

MAE <- mean(abs_diff_test, na.rm = TRUE)
RMSE <- sqrt(mean((flowrate_test_predicted-test.gp$Flowrate.ts)^2 , na.rm = TRUE))
MSE <- RMSE^2
MAPE <- mape(flowrate_test_predicted, test.gp$Flowrate.ts)
R2 <- rSquared(test.gp$Flowrate.ts, resid = abs_diff_test)

cat(" MAE:", MAE, "\n", "RMSE:", RMSE, "\n", "MSE:", MSE, "\n", "MAPE:", MAPE, "\n", "R-squared:", R2)

# Comparision Plots
## PLOT 1
par(mfrow = c(1, 2))
plot(Flowrate.df_sample[1:6000, 2] ~ Flowrate.df_sample[1:6000, 1],
     pch = 16, cex = 0.1, col="red",
     xlab = "Tempo (dias)" , ylab=expression(Predições (m^3/s)))
lines(Flowrate.df_sample[1:6000, 1], flowrate_train_predicted, lwd="1", col="blue")
legend("topright", inset=.02, legend=c("Observações", "Predições"), col=c("red", "blue"), cex=0.75, lty=c(3,1))

plot(Flowrate.df_sample[6001:7788, 2] ~ Flowrate.df_sample[6001:7788, 1],
     pch = 16, cex = 0.1, col="red",
     xlab = "Tempo (dias)" , ylab=expression(Predições (m^3/s)))
lines(Flowrate.df_sample[6001:7788, 1], flowrate_test_predicted, lwd="1", col="blue")
legend("topright", inset=.02, legend=c("Observações", "Predições"), col=c("red", "blue"), cex=0.75, lty=c(3,1))

## PLOT 2
par(mfrow = c(2, 1), mar=c(2,2,2,2))
plot(pred_train.gp ~ train.gp$Flowrate.ts, pch = 16, cex = 0.1, 
     xlab = "Observações (m³/s)" , ylab=expression(Predições (m^3/s)))
reg_train.gp <- lm(pred_train.gp ~ train.gp$Flowrate.ts)
abline(reg_train.gp, col="red")
R2 <- rSquared(train.gp$Flowrate.ts, resid = abs_diff_train)
legend("bottomright", inset=.02, legend=c("Vazão", "Regressão Linear", paste("R2 is", format(R2,digits=3))), col=c("black", "red"), cex=0.75, lty=c(3,1,0))


plot(pred_test.gp ~ test.gp$Flowrate.ts, pch = 16, cex = 0.1, 
     xlab = "Observações (m³/s)" , ylab=expression(Predições (m^3/s)))
reg_test.gp <- lm(pred_test.gp ~ test.gp$Flowrate.ts)
abline(reg_test.gp, col="red")
R2 <- rSquared(test.gp$Flowrate.ts, resid = abs_diff_test)
legend("topright", inset=.02, legend=c("Vazão", "Regressão Linear", paste("R2 is", format(R2,digits=3))), col=c("black", "red"), cex=0.75, lty=c(3,1,0))


# Plots
## Plot 1
par(mfrow = c(1, 1))
plot(pred_test.gp ~ test.gp$Flowrate.ts, pch = 16, cex = 0.1, 
     xlab = "Observações (m³/s)" , ylab=expression(Predições (m^3/s)))
reg_test.gp <- lm(pred_test.gp ~ test.gp$Flowrate.ts)
abline(reg_test.gp, col="red")

## Plot 2
# Plotting time series c-bind
par(mfrow = c(2, 1))

plot(ts(test.gp$Flowrate.ts), pch = 16, cex = 0.1,
     xlab = "Tempo (dias)" , ylab="Vazão Observada (m³/s)")

plot(flowrate_test_predicted, pch = 16, cex = 0.1, col="blue3",
     xlab = "Tempo (dias)" , ylab="Vazão Prevista (m³/s)")

## Plot 3
par(mfrow = c(1, 1))
plot(Flowrate.df_sample[6001:7788, 2] ~ Flowrate.df_sample[6001:7788, 1],
     pch = 16, cex = 0.1, col="red",
     xlab = "Tempo (dias)" , ylab=expression(Predições (m^3/s)))
points(Flowrate.df_sample[6001:7788, 1], flowrate_test_predicted, lwd="1", col="blue")

# FINAL PLOT
x_trainning <- Flowrate.df_sample[1:6000, 1]
y_trainning <- pred_train.gp
trainning_df <- data.frame(x_trainning, y_trainning)[,1:2]


x_predict <- Flowrate.df_sample[6001:7788, 1]
y_predict <- pred_test.gp
predict_df <- data.frame(x_predict, y_predict)


prec_plot_3 <- ggplot() +
  geom_point(data = Flowrate.df_sample, aes(x = Flowrate.df_sample[,1], y = Flowrate.df_sample[,2]), color = "blue", cex = 0.1) +
  geom_line(data = trainning_df, aes(x = trainning_df[,1], y = trainning_df[,2]), color = "yellow") +
  geom_line(data = predict_df, aes(x = predict_df[,1], y = predict_df[,2]), color = "red")


prec_plot_3 + labs(x = "Data", y = "Vazão (m³/s)")




# hist(train.gp$Flowrate.ts, breaks=100, xlim=c(0,20))
# skewness(train.gp$Flowrate.ts)

# data_model <- transform(data_model, lag_1=c(NA, Flowrate.ts[-7792]))
# data_model <- transform(data_model, lag_2=c(NA, lag_1[-7792]))


# TRAIN MODEL ############################
# Tuning set
library(caret)
set.seed(111)
cvCtrl <- trainControl(
  method ="repeatedcv",
  repeats = 1,
  number = 20,
  allowParallel = TRUE,
  verboseIter = TRUE,
  savePredictions = "final")

# Train (aprox. 4 seconds time-simulation)
set.seed(111)
system.time(Model_train <- caret::train(Flowrate.ts ~  .,
                                        trControl = cvCtrl,
                                        data = train.gp,
                                        metric = "MAE", # Using MAE since I intend minimum values are my focus 
                                        preProcess = c("center", "scale"),
                                        method = "gaussprRadial", # Setting RBF kernel function
                                        maxit = 1000,
                                        linout = 1)) # Regression type
pred_test.gp <- predict(Model_train, test.gp, variance.model = TRUE)

flowrate_test_predicted <- ts(pred_test.gp)

# Metrics
abs_diff_test <- abs(flowrate_test_predicted - test.gp$Flowrate.ts)

MAE <- mean(abs_diff_test, na.rm = TRUE)  # erro absoluto médio 
RMSE <- sqrt(mean((flowrate_test_predicted-test.gp$Flowrate.ts)^2 , na.rm = TRUE))  # erro quadrático médio
R2 <- rSquared(test.gp$Flowrate.ts, resid = abs_diff_test)
cat(" MAE:", MAE, "\n", "RMSE:", RMSE, "\n", "R-squared:", R2)

plot(Flowrate.dates[6001:7788,], test.gp$Flowrate.ts, cex=0.3)
lines(Flowrate.dates[6001:7788,], predict(model.gp, test.gp), col="red")

