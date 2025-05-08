# Установка и загрузка необходимых библиотек
#install.packages("dplyr")  # Для обработки данных
#install.packages("ggplot2")  # Для визуализации
library(dplyr)
library(ggplot2)
Sys.setlocale("LC_ALL", "Russian")

# выгрузка данных
data <- read.csv("uniswap1.csv", sep=";", dec=".", header=TRUE)
data <- data %>%
  mutate(log_n = log(data$ActiveUsersMonthly),
         log_V = log(data$MarketCapFullyDiluted))

# Линейная регрессия: log(V) ~ log(n)
model <- lm(log_V ~ log_n, data = data)
summary(model)

# Коэффициенты модели
k_log <- coef(model)[1]  # log(k)
slope <- coef(model)[2]  # Должно быть около 2 по закону Меткалфа
k <- exp(k_log)          # Обратное преобразование для получения k

# Предсказанные значения
data$predicted_V <- exp(predict(model))

# Вывод результатов
cat("Коэффициент k:", k, "\n")
cat("Наклон (должен быть близок к 2):", slope, "\n")

# Визуализация
ggplot(data, aes(x = data$ActiveUsersMonthly, y = data$MarketCapFullyDiluted)) +
  geom_point(color = "blue", size = 3) +  # Точки данных
  geom_line(aes(y = predicted_V), color = "red") +  # Предсказанная кривая
  labs(title = "Закон Меткалфа для криптовалюты",
       x = "Число пользователей (n)",
       y = "Рыночная капитализация (V)") +
  theme_minimal()
