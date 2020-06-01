# descriptive stats
Hmisc::describe(mtcars)
pastecs::stat.desc(mtcars)

# missed values
data(sleep, package = 'VIM')
#install.packages('VIM')
head(sleep)
sleep[!complete.cases(sleep),]

#install.packages('mice')
mice::md.pattern(sleep)
library(mice)

# filling NA values
imp <- mice(sleep, seed = 1234)
fit <- with(imp, lm(Dream ~ Span + Gest))
pooled <- pool(fit)
summary(pooled)

# result
sleep_imp3 <- complete(imp, action = 3)
head(sleep_imp3)

sleep[!complete.cases(sleep_imp3), ]

save(sleep_imp3, file = "sleep_imp.Rdata")
?save
####

# define distribution type
# example 1 
# analytical 
library(MASS)
set.seed(0)
x.gam <- rgamma(200, rate = 0.5, shape = 3.5)

#
med.gam <- mean(x.gam)
var.gam <- var(x.gam)

(l.est <- med.gam / var.gam)
(g.ext <- ((med.gam) ^ 2) / var.gam)

# rootSolve
library(rootSolve)
#install.packages('rootSolve')
f1 <- function(x){
  c(F1 = x[1] / x[2] - med.gam,
    F2 = x[1] / x[2] ^ 2 - var.gam)
}
multiroot(f1, c(3, 0.6))

# example 2
# veibull, shape  = lambda, scale = 1/lambda
set.seed(1946)
library(fitdistrplus)
# install.packages('fitdistrplus')
x <- sort(
  rweibull(100, 2, (1 + 1.21*rbinom(100, 1, 0.05)))
)

summary(x)
hist(x, freq = FALSE, breaks = 15, col = 'grey88', 
     main = "Histogram and kernel density")
lines(density(x), lwd = 2)

graph_distr <- function(x, pc, pd, main_name = ""){
  op <- par(mfrow = c(1, 1), pty = 's')
  par(mfrow = c(1, 2))

  mn <- paste(c("Empirical cumulative function of distribution and ", main_name))
  plot(x, pc, type = 'l', col = 'red', lwd = 2, main = mn)
  plot(ecdf(x), add = TRUE)
  
  mn <- paste(c("Empirical function of density and ", main_name))
  plot(density(x), col = 'blue', lwd = 2, main = mn)
  lines(x, pd, col = 'red', lwd = 2)
  
  par(op)
}

# Normal
(dof = fitdistr(x, 'normal'))
ep1 = dof$estimate[1]; ep2 = dof$estimate[2]
ep1
ep2

ks.test(x, pnorm, mean = ep1, sd = ep2)

graph_distr(
  x,
  pnorm(x, mean = ep1, sd = ep2), 
  dnorm(x, mean = ep1, sd = ep2), 
  ' -- normal distribution'
)

#   Log-normal
(dof = fitdistr(x, 'log-normal'))
ep1 = dof$estimate[1]; ep2 = dof$estimate[2]
ep1
ep2

ks.test(x, plnorm, meanlog = ep1, sdlog = ep2)

graph_distr(
  x, 
  plnorm(x, meanlog = ep1, sdlog = ep2),
  dlnorm(x, meanlog = ep1, sdlog = ep2),
  '-- lognormal distribution'
)

# Veibull
(dof  = fitdistr(x, densfun = dweibull, start = list(scale = 2, shape = 2)))
ep1 = dof$estimate[1]; ep2 = dof$estimate[2]

?fitdistr


# fitdistplus library - stabel with NAN values
library(fitdistrplus)
(dof = fitdist(x, 'weibull')  )
ep1 = dof$estimate[1]; ep2 = dof$estimate[2]

ks.test(x, pweibull, shape = ep1, scale = ep2)
graph_distr(
  x, 
  pweibull(x, scale = ep2, shape = ep1),
  dweibull(x, scale = ep2, shape =ep1), 
  "weibull distribution"
)


x <- c(12, 20, 19, 19, 18, 10, 19, 30, 16, 10, 8, 11, 10, 11,
  16, 3, 7, 6, 5, 11, 8, 14, 9, 8, 10, 11, 14, 17, 2, 7,
  17, 19, 9, 15, 9, 8, 4, 8, 11, 8, 5, 3, 10, 14, 22, 11,
  8, 7, 3, 5, 8, 11, 14, 2, 13, 9, 12, 6, 19, 21)

x
table(x)

# visual and Kolgomogorov - Smirnov test

n = length(x); p1 = mean(x); p2 = sqrt(var(x) * (n-1)/n)
pr_obs <- as.vector(table(x)/n); nr  <- length(pr_obs)
pr_obs

# data from theoretical distribution
pr_norm <- dnorm(1 : nr, p1, p2)
pr_norm

pr_pois <- dpois(1:nr, p1)
pr_pois

# chart
plot(pr_obs, type = 'b', ylab = 'Freqs')
lines(1:nr, pr_pois, col = 'red', lwd = 2)
lines(1:nr, pr_norm, col = 'blue', lwd = 2)
legend('topright', legend = c('Normal', 'Poisson'),
       lwd = 2, col = c('red', 'blue'))

ks.test(pr_obs, pr_norm)$statisitic
#





# vcd lib
library(vcd)
gf <- goodfit(table(x), type = 'poisson', method = 'ML')
summary(gf)
?goodfit


##### NORMAL DISTRIBUTION testing

# Normal distribution estimating on chart
#install.packages('sm')
library(sm)
sm.density(x, model = 'Normal', 
           xlab = 'Imitated sample', 
           ylab = 'Function density distribution')


# 
x =sort( rweibull(100, 2, (1 + 1.21 * rbinom(100, 1, 0.05))))
x
# install.packages('car')
library(car)
qqPlot(x, dist = 'norm', col = palette()[1], pch = 19, 
       xlab = 'normal', 
       ylab = 'observed', 
       main = 'quantiles comparison')


#####################
x <- sort(
  rweibull(100, 2, (1 + 1.21*rbinom(100, 1, 0.05)))
)

#### Formal  Normality TESTs

# Shapiro-Uilco
shapiro.test(x)
# The null-hypothesis of this test is that the population is normally distributed. Thus, if the p value is less than the chosen alpha level, then the null hypothesis is rejected

#install.packages('nortest')
library(nortest)

# Anderson-Darlling
ad.test(x)

# Cramer-von Mises
cvm.test(x)

# Kolmogorov-Smirnov with Lilieforce addition 
lillie.test(x)
?ad.test
?cvm.test
?lillie.test

###############################################################
