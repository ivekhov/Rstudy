install.packages("gamair")
library(gamair)
data(hubble)
str(hubble)

M <- lm(y ~ x - 1, data = hubble)
summary(M)
(hub.const <- 76.581 / 3.09e19)
(age <- 1/ hub.const)
age / (60 ^ 2 * 24 * 365)

# Confidence level of Regression model -  CI
# Prediction Interval  -                  PI

CPI.df <- cbind(predict(M,interval ="conf"), predict(M,interval ="pred"))

CPI.df <- CPI.df[,-4] 
head(CPI.df)
colnames(CPI.df) <- c("Y_fit","CI_l","CI_u","PI_l","PI_u")
head(CPI.df)

matplot(hubble$x, CPI.df, type = "l", 
        lwd = c(2, 1, 1, 1, 1), col = c(1, 2, 2, 4, 4),
        ylab = "Скорость,км/с",xlab="Расстояние,Мпс")
with(hubble, matpoints(x, y, pch = 20))


###############################################################################
# How estimate model quality 

# 1 Parameters 


# Оценка доверительных интервалов параметра:
beta <- summary(M)$coefficients[1]
SE <- summary(M)$coefficients[2]
ci.lower <- beta - qt(0.975, df = 23)*SE
ci.upper <- beta + qt(0.975, df = 23)*SE
c(ci.lower, ci.upper)
Uni.upper <- 1/(ci.lower*60^2*24*365.25/3.09e19)
Uni.lower <- 1/(ci.upper*60^2*24*365.25/3.09e19)
c(Uni.lower, Uni.upper)


library(gamair)
data(hubble)
##


# 2 Bootstap

library(gamair)
library(ggplot2)
data(hubble)

boots = vector(mode="list", length=6)

for(i in 1:6){
  boots[[i]] = hubble[sample(1:24, 24, replace = TRUE), 2:3]  
}

boots = do.call(rbind.data.frame, boots)
boots$reps = rep(c("A", "B", "C", "D", "E", "F"), each = 24)

ggplot(boots, aes(x, y)) + geom_point() + facet_wrap(~reps)
#

# BOOTSTRAP
regr <- function(data, indices) {
  # вектор indices будет формироваться функцией boot() 
  dat <- data[indices, ] 
  fit <- lm(y ~ -1 + x, data = dat)
  return(summary(fit)$coefficients[1])
}


library(boot)
results <- boot(data = hubble, statistic = regr, R = 1000)
results
plot(results) # Рис. 90
quantile(results$t, c(0.025, 0.975))
U.lower <- 1/(85.73249*60^2*24*365.25/3.09e19)
U.upper <- 1/(67.07360*60^2*24*365.25/3.09e19)
U.lower
U.upper
boot.ci(results, type = "bca")
#

# 3 Imitaion procedures

# Оценка доверительных интервалов параметра методом имитаций:
library(arm)
simulations <- sim(M, 1000)
hist(simulations@coef, breaks = 30)
sd(simulations@coef)
quantile(simulations@coef, c(0.025, 0.975))
#

anova(M)

M <- lm(y ~ x - 1, data = hubble)
hubble$fit = fitted(M)

p1 = ggplot(hubble, aes(x, y)) + geom_point() +
  geom_hline(aes(yintercept=mean(hubble$y)), color = "blue") +
  geom_segment(aes(x = x, y = y, xend = x, yend = mean(hubble$y))) +
  ggtitle("TSS")

p2 = ggplot(hubble, aes(x, y)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_segment(aes(x = x, y = y, xend = x, yend = fit)) +
  ggtitle("RSS")

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)
# TSS, RSS evaluation 



# R^2, adj R^2

summary(M)
#############################################################################

# p.305 7.3 Model diagnostics Methods 

# 2 ToDo 
# Одно из наиболее важных допущений при работе с линейными моделями, параметры 
# которых оцениваются методом наименьших квадратов, состоит в том, что  
# (1) остатки модели независимы (то есть не коррелируют) 
# (2) имеют нормальное распределение со средним значением 0 и некоторым 
# фиксированным стандартным отклонением сигме, то есть остатки распределены 
# нормально со средней в 0 и станд.отклонение == сигме .  
# == требованию, что дисперсия остатков модели должна быть однородна

# (1) Для проверки наличия корреляции в остатках можно использовать графики 
# автокорреляционной функции, а также соответствующие формальные тесты 
# (в частности, тест Дарбина-Уотсона, реализованный, например, в пакете lmtest).

# (2)  для проверки нормальности распределения количественных переменных можно 
# использовать визуальный анализ гистограмм и квантильных графиков, а также 
# применять формальные методы вроде теста Шапиро-Уилка.

# Однородность дисперсии остатков 
# Чаще всего данное
# условие проверяется графическим способом. Прежде всего строится график, на
# котором по оси абсцисс откладывают предсказанные моделью значения зависи-
# мой переменной, а по оси ординат – соответствующие значения остатков. Если
# условие однородности дисперсии выполняется, то на таком графике точки будут
# располагаться совершенно случайно относительно горизонтальной линии y = 0,
# без проявления каких-либо закономерностей

# ОДНОРОДНОСТЬ ДИСПЕРСИИ ОСТАТКОВ :  Рис. 92:

par(mfrow = c(3, 3))
set.seed(101)
for (i in 1:9) plot(1:50, rnorm(50),
                    xlab = "Предсказанные значения",
                    ylab = "Остатки")

# ЯВНО ВЫВРАЖЕННОЕ ВОЗРАСТАНИЕ ДИСПЕРСИИ ОСТАТКОВ   Рис. 93:
par(mfrow = c(3, 3), mar = c(4.5, 4.4, 2, 2))
set.seed(101) # для воспроизводимости кода
for (i in 1:9) plot(1:50, (1:50)*rnorm(50),
                    xlab = "Предсказанные значения",
                    ylab = "Остатки")

# УМЕРЕННОЕ ВОЗРАСТАНИЕ ДИСПЕРСИИ ОСТАТКОВ Рис. 94: 
par(mfrow = c(3, 3), mar = c(4.5, 4.4, 2, 2))
set.seed(101) # для воспроизводимости кода
for (i in 1:9) plot(1:50, sqrt((1:50))*rnorm(50),
                    xlab = "Предсказанные значения",
                    ylab = "Остатки")

# Рис. 95:
x = 1:50
set.seed(200)
y = rnorm(50, 0.1 + 0.2*x + 0.035*(x^2), 10)
plot(x, y)

# рекомендуется также анализировать графики, где по оси X от-
# кладывают значения каждого из включенных и не включенных в модель предикто-
# ров. При наличии проблем с неоднородностью дисперсии остатков этот подход
# позволит выявить «проблемные» предикторы.

# формальные методы проверки условия однородности дисперсии остатков
# car::ncvTest(MODEL)
# проверяется нулевая  гипотеза о том, что
# дисперсия остатков никак не связана с предсказанными моделью значениями
library(car)
library(gamair)

data(hubble)
M <- lm(y ~ x - 1, data = hubble); M1 <- lm(y ~ x, data = hubble)
car::ncvTest(M); 
car::ncvTest(M1); 
#####














