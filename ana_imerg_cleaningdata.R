# Loading libraries
library(dplyr)  # select function
library(tidyverse)  # gather function
library(tidyquant)
library(tibble)  # pipe %>%
library (ggplot2)
library(mice)
library(sysfonts)
library(tseries)


font_add_google('Merriweather')

################## Selecting and filtering flowrate (ANA) - target ##################

# Reading flowrate data from ANA csv
Flowrate.df <- read.table("C:/Users/Ana Carolina/Documents/tcc/sub-bacias/fazenda_boa_esperanca/ana_flowrate/fazenda_boa_esperanca.csv", sep=";", header=TRUE)

# Selecting and filtering available data
Flowrate.df <- data.frame(Flowrate.df[c(16:46)])[c(436:698),]
Flowrate.df <- as_tibble(Flowrate.df)
Flowrate.df <- tibble::rowid_to_column(Flowrate.df, "ID")

# Rearranging data
Flowrate.df <- gather(Flowrate.df, key, val, - ID, na.rm = TRUE) %>% select(-key) %>% arrange(ID)

# Available days (looked into excel)
days_seq_1 <- seq(as.Date("2000/06/01"), as.Date("2015/12/10"), by=1)
days_seq_2 <- seq(as.Date("2016/01/14"), as.Date("2016/12/31"), by=1)
days_seq_3 <- seq(as.Date("2017/02/12"), as.Date("2021/09/30"), by=1)
days_seq_4 <- seq(as.Date("2021/12/01"), as.Date("2022/07/31"), by=1)

days_seq_f_1 <- c(days_seq_1, days_seq_2, days_seq_3, days_seq_4)
days_seq_f <- seq(as.Date("2000/06/01"), as.Date("2021/09/30"), by=1)

Flowrate.df <- tibble(days_seq_f_1, Flowrate.df[2])
colnames(Flowrate.df)[1] <- "Date"
colnames(Flowrate.df)[2] <- "Flowrate"

# Not available days
days_seq_f_2 <- as.Date(setdiff(days_seq_f, days_seq_f_1))
Flowrate.na <- tibble(days_seq_f_2, NA)
colnames(Flowrate.na)[1] <- "Date"
colnames(Flowrate.na)[2] <- "Flowrate"

# Final Flowrate dataframe
Flowrate.df <- rbind(Flowrate.na, Flowrate.df)
Flowrate.df <- Flowrate.df[order(Flowrate.df$Date),][1:7792,]  # setting same precipitation length

# Handle Missing Values: Imputation using R (mice)
methods(mice)
my_input <- mice(Flowrate.df, m=5, method=c("","pmm"), seed=500, maxit = 15)
my_input$imp$Flowrate

mean(unlist(my_input$imp$Flowrate[2]))
final <- complete(my_input, 2)

Flowrate.ts <- ts(final$Flowrate)  # PMM Imputation
Flowrate.df <- data.frame(Flowrate.df$Date, Flowrate.ts)

################## Selecting avg precipitation from IMERG (NASA) - predictors ################## 

# Reading precipitation data from NASA csv
Precipitation.df <- read.table("C:/Users/Ana Carolina/Documents/tcc/sub-bacias/fazenda_boa_esperanca/gpm_imerg_csv/AveragePrecipitation.csv", sep=",", header=TRUE)

days_seq_p <- seq(as.Date("2000/06/01"), as.Date("2021/09/30"), by=1)
days_seq_p <- as_tibble(days_seq_p, "%Y%m%d")

Precipitation.df["Date"] <- days_seq_p
Precipitation.ts <- ts(Precipitation.df$AveragePrecipitation)

# Generating data-series
series <- data.frame(Precipitation.ts, Flowrate.ts)

#################################### Plots #################################### 
# Plotting time series c-bind
par(mfrow = c(2, 1), mar = c(2,2,2,2))

plot(Precipitation.ts, pch = 16, cex = 0.1, col="blue",
     xlab = "Tempo (dias)" , ylab="Precipitação Média Observada (mm)")

plot(Flowrate.ts, pch = 16, cex = 0.1, col="red",
     xlab = "Tempo (dias)" , ylab="Vazão Observada (m³/s)")

par(mfrow = c(1, 1))
plot(Flowrate.ts ~ Precipitation.ts, pch = 16, cex = 0.1, 
     xlab = "Precipitação (mm)" , ylab="Vazão (m³/s)")
reg_test.gp <- lm(Flowrate.ts ~ Precipitation.ts)
abline(reg_test.gp, col="red")

# Plotting histograms
hist(Flowrate.ts, breaks=100)
hist(Precipitation.ts, breaks=100)

################################# Time-series analysis #################################
# library(fpp2)
# 
# # Autocorrelação entre as séries temporais
# par(mfrow = c(2, 1))
# acf(Precipitation.ts,)
# acf(Flowrate.ts,)

# par(mfrow = c(2, 1))
# Precipitation.1 <- diff(Precipitation.ts, differences = 1)
# acf(Precipitation.1, )
# Precipitation.2 <- diff(Precipitation.ts, differences = 2)
# acf(Precipitation.2, )
# 
# par(mfrow = c(2, 1))
# Flowrate.1 <- diff(Flowrate.ts, differences = 1)
# acf(Flowrate.1, )
# Flowrate.2 <- diff(Flowrate.ts, differences = 2)
# acf(Flowrate.2, )

# # Análise de correlação cruzada das séries diferenciadas
# par(mfrow = c(2, 1))
# ccf(Precipitation.ts, Flowrate.ts)
# ccf(Precipitation.2, Flowrate.2)

# library(xts)
# teste <- ts(Flowrate.df, frequency=1, start=c(2000,6))
# 
# Flowrate_dec.ts <- xts(Flowrate.df, order.by=as.Date(Flowrate.df[,1], "%d/%m/%Y"))
# Precipitation_dec.ts <- xts(Precipitation.df[,-1], order.by=as.Date(Precipitation.df[,1], "%d/%m/%Y"))
# 
# # Modelagem Aditiva
# decomposeAditiva.f <- decompose(
#   x = teste,
#   type = "additive"
# )
# 
# decomposeAditiva.p <- decompose(
#   x = Precipitation_dec.ts,
#   type = "additive"
# )
# 
# 
# Flowrate.1 <- decomposeAditiva.p$random
# Precipitation.1 <- decomposeAditiva.p$random
# 
# # Grafico da decomposicao da modelagem aditiva
# plot(decomposeAditiva.p$random)

# Plots
# # Plotting time series for each parameter
# ## Flowrate
# flowrate_plot <- ggplot(data = Flowrate.df, aes(x = Date, y = Flowrate, group=1)) + 
#   scale_x_date(date_breaks = "5 years", date_labels = "%b-%Y") +
#   theme(text=element_text(size=16,  family="Merriweather")) +
#   geom_line(color = "#00AFBB", size = 0.5) + 
#   labs(x="Date",y="Flowrate(m³/s)")
#   
# flowrate_plot + theme_classic()
# 
# ggsave("plots/flowrate_plot.png", flowrate_plot, width = 10, dpi = 300)
# 
# ## Precipitation
# precipitation_plot <- ggplot(data = Precipitation.df, aes(x = Date, y = AveragePrecipitation, group=1)) + 
#   scale_x_date(date_breaks = "5 years", date_labels = "%b-%Y") +
#   theme(text=element_text(size=16,  family="Merriweather")) +
#   geom_line(color = "#00AFBB", size = 0.5) + 
#   labs(x="Date",y="Precipitation(mm)")
# 
# precipitation_plot + theme_classic()
# 
# ggsave("plots/precipitation_plot.png", precipitation_plot, width = 10, dpi = 300)

# max.len <- max(length(days_seq_f), length(Flowrate.df$val))
# days_seq_f <- c(days_seq_f, rep(NA, max.len - length(days_seq_f)))
# Flowrate.df <- c(Flowrate.df$val, rep(NA, max.len - length(Flowrate.df$val)))

# days_seq_f <- as_tibble(days_seq_f, "%Y%m%d")

# Missing data strategies
# R Tidyverse Fill Missing Values (last observation carried foward)
# Flowrate.ts.tst <- tibble(Flowrate.ts) %>% mutate(Filled=na.locf(Flowrate.ts))
# Handle Missing Values: Imputation using R (mean)
# Flowrate.ts[which(is.na(Flowrate.ts))] <- mean(Flowrate.ts, na.rm=TRUE)

## Plotting cross-correlation between Flowrate and Precipitation (revisar !!!!!!!!!!)
### Correlação cruzada entre 01/01/2007 e 01/01/2008
# Flowrate.df_s <- Flowrate.df[2406:2771,]
# Precipitation.df_s <- Precipitation.df[2406:2771,]
# 
# ccf(Precipitation.df_s[2], Flowrate.df_s[2], main="Correlação cruzada entre 01/01/2007 e 01/01/2008")
# 
# ### Correlação cruzada entre 01/01/2008 e 01/01/2009
# Flowrate.df_s <- Flowrate.df[2771:3137,]
# Precipitation.df_s <- Precipitation.df[2771:3137,]
# 
# ccf(Precipitation.df_s[2], Flowrate.df_s[2], main="Correlação cruzada entre 01/01/2008 e 01/01/2009")
# 
# ### Correlação cruzada entre 01/01/2009 e 01/01/2010
# Flowrate.df_s <- Flowrate.df[3137:3502,]
# Precipitation.df_s <- Precipitation.df[3137:3502,]
# 
# ccf(Precipitation.df_s[2], Flowrate.df_s[2], main="Correlação cruzada entre 01/01/2010 e 01/01/2010")


# Flowrate.ts_NA <- ts(Flowrate.final$Flowrate)  # Missing Data
  
# flow_d1 <- diff(Flowrate_dec.ts, differences = 1)
# prec_d1 <- diff(Precipitation_dec.ts, differences = 1)
# 
# ## Plot the differenced data
# plot(flow_d1, ylab = expression(paste(nabla^1, "flowrate"[1])))
# plot(prec_d1, ylab = expression(paste(nabla^1, "precipitation"[1])))
# series_d1 <- data.frame(prec_d1, flow_d1)
# 
# Flowrate_dec.ts <- ts(Flowrate.df$Flowrate.ts, start=c(2000,1), end=c(2021,9), frequency=365)
# Precipitation_dec.ts <- ts(Precipitation.df$AveragePrecipitation, start=c(2000,1), end=c(2021,9), frequency=365)
# 
# # Modelagem Aditiva
# decomposeAditiva <- decompose(
#   x = Flowrate_dec.ts,
#   type = "additive"
# )

# # Grafico da decomposicao da modelagem aditiva
# plot(
#   decomposeAditiva
# )
#  
# 
# adf.test(series$Flowrate.ts)  # p-valor < 5%
# adf.test(series$Precipitation.ts)  # p-valor < 5%


# # Performing SES on  the Flowrate data
# Flowrate.ses <- ses(Flowrate.ts, 
#                 alpha = .2,
#                 h = 100)
# autoplot(Flowrate.ses)
# 
# Precipitation.ses <- ses(Precipitation.ts, 
#                     alpha = .2,
#                     h = 100)
# autoplot(Flowrate.ses)
# 
# 
# # applying holt's method on
# # Google stock Data
# holt.goog <- holt(Flowrate.ts,
#                   h = 100)
# autoplot(holt.goog)
# 
# # reapplying SES on the filtered data
# series_d1 <- ses(goog.dif,
#                     alpha = .2, 
#                     h = 100)
# autoplot(ses.goog.dif)
# 


# 
# series_d1 <- data.frame(Precipitation.ts[1:7791], Flowrate.1)
# colnames(series_d1) <- c('Precipitation.1', 'Flowrate.1')
# series_d2 <- data.frame(Precipitation.ts[1:7790], Flowrate.2)
# colnames(series_d2) <- c('Precipitation.2', 'Flowrate.2')
