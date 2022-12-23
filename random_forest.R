# Loading libraries
library(randomForest)

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
train.rf <- data_model[1:6000, c(2, 4:6)]
test.rf <- data_model[6001:7788, c(2, 4:6)]

# Random Forests
## Customizing manually ntree
set.seed(1)

model.rf <- randomForest(Flowrate.ts ~ ., data=train.rf, na.action=na.omit,
                         ntree=1000, nodesize=0.0001, importance=TRUE,
                         proximity=TRUE,
                         replace = FALSE)

print(model.rf)
plot(model.rf, main="")

########################### Prediction – train data ###########################
pred_train.rf <- predict(model.rf, train.rf)
flowrate_train_predicted <- ts(pred_train.rf)

# Metrics
abs_diff_train <- abs(flowrate_train_predicted - train.rf$Flowrate.ts)

MAE <- mean(abs_diff_train, na.rm = TRUE)
RMSE <- sqrt(mean((flowrate_train_predicted-train.rf$Flowrate.ts)^2 , na.rm = TRUE))
MSE <- RMSE^2
MAPE <- mape(flowrate_train_predicted, train.rf$Flowrate.ts)
R2 <- rSquared(train.rf$Flowrate.ts, resid = abs_diff_train)

cat(" MAE:", MAE, "\n", "RMSE:", RMSE, "\n", "MSE:", MSE, "\n", "MAPE:", MAPE, "\n", "R-squared:", R2)


########################### Prediction – test data ###########################
pred_test.rf <- predict(model.rf, test.rf)
flowrate_test_predicted <- ts(pred_test.rf)

# Metrics
abs_diff_test <- abs(flowrate_test_predicted - test.rf$Flowrate.ts)

MAE <- mean(abs_diff_test, na.rm = TRUE)
RMSE <- sqrt(mean((flowrate_test_predicted-test.rf$Flowrate.ts)^2 , na.rm = TRUE))
MSE <- RMSE^2
MAPE <- mape(flowrate_test_predicted, test.rf$Flowrate.ts)
R2 <- rSquared(test.rf$Flowrate.ts, resid = abs_diff_test)

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
plot(pred_train.rf ~ train.rf$Flowrate.ts, pch = 16, cex = 0.1, 
     xlab = "Observações (m³/s)" , ylab=expression(Predições (m^3/s)))
reg_train.rf <- lm(pred_train.rf ~ train.rf$Flowrate.ts)
abline(reg_train.rf, col="red")
R2 <- rSquared(train.rf$Flowrate.ts, resid = abs_diff_train)
legend("bottomright", inset=.02, legend=c("Vazão", "Regressão Linear", paste("R2 is", format(R2,digits=3))), col=c("black", "red"), cex=0.75, lty=c(3,1,0))


plot(pred_test.rf ~ test.rf$Flowrate.ts, pch = 16, cex = 0.1, 
     xlab = "Observações (m³/s)" , ylab=expression(Predições (m^3/s)))
reg_test.rf <- lm(pred_test.rf ~ test.rf$Flowrate.ts)
abline(reg_test.rf, col="red")
R2 <- rSquared(test.rf$Flowrate.ts, resid = abs_diff_test)
legend("topright", inset=.02, legend=c("Vazão", "Regressão Linear", paste("R2 is", format(R2,digits=3))), col=c("black", "red"), cex=0.75, lty=c(3,1,0))


library(datasets)
library(caret)
library(party)
library(formattable)
library(caTools)
library(miscTools)



# Utils
# getTree(model.rf, 1)

# plot_tree <- ctree(Flowrate.ts ~ ., data=series)
# plot(plot_tree, type="simple")
# plot(cbind(ts(pred_train), train))
# varImpPlot(model.rf)

