# lab 2
library("memisc")
library("dplyr")
library("psych")
library("lmtest")
library("sjPlot")
library("sgof")
library("ggplot2")
library("foreign")
library("car")
library("hexbin")

# Задача 1: генерирование случайных величин
# z_1, ... z_100 ~ N(5,9)
z <- rnorm(100, mean = 5, sd = 9)
z[56]
z[2:9]
qplot(z)

# Задача 2: построение функции плотности
x <- seq(-10, 15,by=0.5)
y <- dnorm(x, mean=5, sd=3)
qplot(x, y)
qplot(x,y,geom="line")

# Задача 3: P(z<3)
# P(z<3)=F(3)
pnorm(3,mean=5, sd=3)

# P(x \in [4; 9])
pnorm(9, mean=5,sd=3) - pnorm(4, mean=5, sd=3)

# P(z<a)=0.7 a ? - нахождение квантиля
qnorm(0.7, mean=5, sd=3)

# chisq, t, f

#rchisq, dhisq, pchisq, qhisq
#rt, dt, pt, qt
#rf, df, pf, qf

# Множественная регрессия
h <- swiss
glimpse(h)
help(swiss)

model <- lm(data=h, Fertility~Catholic+Agriculture+Examination)
summary(model)

coeftest(model) # Получение коэффициентов
confint(model) # Доверительный интервал
plot_model(model)

# Проверка линейной гипотезы
# Проверка гипотезы b_Cath = b_agri
model_aux <- lm(data=h, Fertility~Catholic+I(Catholic+Agriculture)+Examination)
summary(model_aux) # Гипотеза не отвергается

# Анологичный способ проверить гипотезу
linearHypothesis(model, "Catholic-Agriculture=0") # Гипотеза не отвергается

# Стандартизированные коэффициенты, оценка силы влияния -существенность
h_st <- mutate_each(h, "scale")
glimpse(h_st)
model_st = lm(data=h_st, 
              Fertility~Catholic+Agriculture+Examination)
summary(model_st)
plot_model(model_st)

# Искусственный эксперимент
D <- matrix(nrow=100, rnorm(100*41, mean=0, sd=1))
df <- data.frame(D)
glimpse(df)
model_pusto <- lm(data=df, X1~. )
summary(model_pusto)


# Сравнить несколько моделей
model_2 <- lm(data=h, Fertility~Catholic+Agriculture)
compar_12 <- mtable(model, model_2)
compar_12

# Сохранение результатов работы
stuff <- list(data=h, model=model2)
saveRDS(file = "mydata.RDS", stuff)

# Загрузка данных
mylist <- readRDS("mydata.RDS")
summary(mylist$model)

# csv - comma separated values
t <- read.csv("flats_moscow.txt")
glimpse(t)

t <- read.csv("flats_moscow.txt", sep = "\t", dec=".", header=TRUE)
glimpse(t)

model_3 <- lm(data=t, price~totsp+brick)
summary(model_3)
