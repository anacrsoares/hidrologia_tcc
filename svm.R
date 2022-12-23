# Loading libraries
library(e1071)
library(miscTools)
library(caret)
library(kernlab)

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
train.svm <- data_model[1:6000, c(2, 4:6)]
test.svm <- data_model[6001:7788, c(2, 4:6)]


# Support Vector Regression
model.svm <- svm(Flowrate.ts ~ ., data = train.svm, 
                 type = "eps-regression", kernel= "radial", cost = 100, gamma=5000, 
                 epsilon=0)  # tolerance = 0.001, cost = 100, gamma=0.5

summary(model.svm)
########################### Prediction – train data ###########################
pred_train.svm <- predict(model.svm, train.svm)
flowrate_train_predicted <- ts(pred_train.svm)

# Metrics
abs_diff_train <- abs(flowrate_train_predicted - train.svm$Flowrate.ts)

MAE <- mean(abs_diff_train, na.rm = TRUE)
RMSE <- sqrt(mean((flowrate_train_predicted-train.svm$Flowrate.ts)^2 , na.rm = TRUE))
MSE <- RMSE^2
MAPE <- mape(flowrate_train_predicted, train.svm$Flowrate.ts)
R2 <- rSquared(train.svm$Flowrate.ts, resid = abs_diff_train)

cat(" MAE:", MAE, "\n", "RMSE:", RMSE, "\n", "MSE:", MSE, "\n", "MAPE:", MAPE, "\n", "R-squared:", R2)

# # Plots
# ## Plot 1
# plot(pred_train.svm ~ train.svm$Flowrate.ts, pch = 16, cex = 0.1, 
# xlab = "Observações (m³/s)" , ylab=expression(Predições (m^3/s)))
# reg_train.svm <- lm(pred_train.svm ~ train.svm$Flowrate.ts)
# abline(reg_train.svm, col="red")
# 
# ## Plot 2
# # Plotting time series c-bind
# par(mfrow = c(2, 1))
# 
# plot(ts(train.svm$Flowrate), pch = 16, cex = 0.1,
#      xlab = "Tempo (dias)" , ylab="Vazão Observada (m³/s)")
# 
# plot(flowrate_train_predicted, pch = 16, cex = 0.1, col="blue3",
#      xlab = "Tempo (dias)" , ylab="Vazão Prevista (m³/s)")
# 
# plot(cbind(flowrate_train_predicted, train.svm$Flowrate), xlab = "Tempo (dias)")
# 
# ## Plot 3
# par(mfrow = c(1, 1))
# plot(Flowrate.df_sample[1:2575, 2] ~ Flowrate.df_sample[1:2575, 1],
#     pch = 16, cex = 0.1, col="red",
#     xlab = "Tempo (dias)" , ylab=expression(Predições (m^3/s)))
# lines(Flowrate.df_sample[1:2575, 1], pred_train.svm, lwd="1", col="blue")

########################### Prediction – test data ###########################
pred_test.svm <- predict(model.svm, test.svm)
flowrate_test_predicted <- ts(pred_test.svm)

# Metrics
abs_diff_test <- abs(flowrate_test_predicted - test.svm$Flowrate.ts)

MAE <- mean(abs_diff_test, na.rm = TRUE)
RMSE <- sqrt(mean((flowrate_test_predicted-test.svm$Flowrate.ts)^2 , na.rm = TRUE))
MSE <- RMSE^2
MAPE <- mape(flowrate_test_predicted, test.svm$Flowrate.ts)
R2 <- rSquared(test.svm$Flowrate.ts, resid = abs_diff_test)

cat(" MAE:", MAE, "\n", "RMSE:", RMSE, "\n", "MSE:", MSE, "\n", "MAPE:", MAPE, "\n", "R-squared:", R2)

# Comparision Plots
## PLOT 1
par(mfrow = c(1, 2), mar=c(2,2,2,2))
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
plot(pred_train.svm ~ train.svm$Flowrate.ts, pch = 16, cex = 0.1, 
     xlab = "Observações (m³/s)" , ylab=expression(Predições (m^3/s)))
reg_train.svm <- lm(pred_train.svm ~ train.svm$Flowrate.ts)
abline(reg_train.svm, col="red")
R2 <- rSquared(train.svm$Flowrate.ts, resid = abs_diff_train)
legend("bottomright", inset=.02, legend=c("Vazão", "Regressão Linear", paste("R2 is", format(R2,digits=3))), col=c("black", "red"), cex=0.75, lty=c(3,1,0))


plot(pred_test.svm ~ test.gp$Flowrate.ts, pch = 16, cex = 0.1, 
     xlab = "Observações (m³/s)" , ylab=expression(Predições (m^3/s)))
reg_test.gp <- lm(pred_test.svm ~ test.svm$Flowrate.ts)
abline(reg_test.gp, col="red")
R2 <- rSquared(test.gp$Flowrate.ts, resid = abs_diff_test)
legend("topright", inset=.02, legend=c("Vazão", "Regressão Linear", paste("R2 is", format(R2,digits=3))), col=c("black", "red"), cex=0.75, lty=c(3,1,0))



# Plots
## Plot 1
plot(pred_test.svm ~ test.svm$Flowrate.1, pch = 16, cex = 0.1, 
    xlab = "Observações (m³/s)" , ylab=expression(Predições (m^3/s)))
reg_test.svm <- lm(pred_test.svm ~ test.svm$Flowrate.1)
abline(reg_test.svm, col="red")

## Plot 2
par(mfrow = c(2, 1))

plot(ts(test.svm$Flowrate.2), pch = 16, cex = 0.1,
     xlab = "Tempo (dias)" , ylab="Vazão Observada (m³/s)")

plot(flowrate_test_predicted, pch = 16, cex = 0.1, col="blue3",
     xlab = "Tempo (dias)" , ylab="Vazão Prevista (m³/s)")

## Plot 3
par(mfrow = c(1, 1))
plot(Flowrate.df_sample[2576:4288, 2] ~ Flowrate.df_sample[2576:4288, 1],
    pch = 16, cex = 0.1, col="red",
    xlab = "Tempo (dias)" , ylab=expression(Predições (m^3/s)))

lines(Flowrate.df_sample[2576:4288, 1], pred_test.svm, lwd="1", col="blue")


# FINAL PLOT
x_trainning <- Flowrate.df_sample[1:6000, 1]
y_trainning <- pred_train.svm
trainning_df <- data.frame(x_trainning, y_trainning)[,1:2]
trainning_df["actual_flowrate"] <- train.svm$Flowrate.ts
trainning_df["diff"] <- y_trainning-train.svm$Flowrate.ts
# trainning_df["%diff"] <- percent(y_trainning/train$Flowrate.ts)

x_predict <- Flowrate.df_sample[6001:7669, 1]
y_predict <- pred_test.svm
predict_df <- data.frame(x_predict, y_predict)
predict_df["actual_flowrate"] <- test.svm$flow_d1
predict_df["diff"] <- y_predict-test.svm$flow_d1
# predict_df["%diff"] <- percent(y_predict/test$Flowrate.ts)

Flowrate.df_sample <- Flowrate.df[3502:7789,]
prec_plot_3 <- ggplot() +
 geom_point(data = Flowrate.df_sample, aes(x = Flowrate.df_sample[,1], y = Flowrate.df_sample[,2]), color = "blue", cex = 0.1) +
 geom_line(data = trainning_df, aes(x = trainning_df[,1], y = trainning_df[,2]), color = "yellow") +
 geom_line(data = predict_df, aes(x = predict_df[,1], y = predict_df[,2]), color = "red")


prec_plot_3 + labs(x = "Data", y = "Vazão (m³/s)")

