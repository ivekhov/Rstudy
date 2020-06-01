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



