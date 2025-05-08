#пакеты для временных рядов
#install.packages("zoo")
#install.packages("xts")
#install.packages("lubridate")
#install.packages("urca")

#пакеты для финансовых данных
#install.packages("quantmod")

# доп пакеты
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("forecast")

library("zoo")
library("xts")
library("lubridate")
library("urca")
library("quantmod")
library("dplyr")
library("ggplot2")
library("forecast")

####################################
# Часть 1. ARIMA
####################################
data<-read.csv("ethereum.csv", sep=";", dec=".",header=TRUE)

# график
price <- data$Price
plot(price, type="line")

mean(price)
acf(price)
pacf(price)
tsdisplay(price)

# Тест Дики-Фулера
df_test<-ur.df(price, type="drift", selectlags="BIC")# H0- нестационарность, H1- стационарность, eth стационарен на 5% уровне значимости
summary(df_test) # 

# auto.arima
best_model<-auto.arima(price)
summary(best_model)

# Остатки модели
residuals <- residuals(best_model)

# Визуализация остатков
plot(residuals, main="Остатки модели ARIMA")

# ACF остатков
acf(residuals, main="ACF остатков")

Box.test(residuals, lag = 10, type = "Ljung-Box")# Если p-значение > 5% , остатки независимы — модель хорошая

prognoz<-forecast(best_model, h=10)
plot(prognoz, xlim=c(100,126))
lines(best_model$fitted, col="red") # добавил расчётные значения для предыдущих периодов



