# Student t-test
d.intake <- c(5260, 5470, 5640, 6180, 6390, 6515, 6805, 7515, 7515, 8230, 8770)
mean(d.intake)
t.test(d.intake, mu = 7734)
library(ISwR)
data("energy")
attach(energy)
head( energy)
tapply(expend, stature, mean)
t.test(expend ~ stature)
t.test(expend ~ stature, var.equal = TRUE)
data("intake")
attach(intake)
mean(post - pre)
t.test(post, pre, paired = TRUE)

####
wilcox.test(d.intake, mu = 7724)
wilcox.test(expend ~ stature, paired = FALSE)
wilcox.test(post, pre, paired = TRUE)
wilcox.test(post, pre, paired = TRUE, conf.int = TRUE)

### permutation and bootstrap
simP <- function(x, group, permutations = 5000) {
  n <- length(x)
  replicate(permutations, {
    xr <- sample(x, n)
    t.test(xr ~ group)$statistic
  })
}

bootP <- function(x, group, boots = 5000) {
  n <- length(x)
  replicate(boots, {
    ind <- sample.int(n, n, replace = T)
    t.test(x[ind] ~ group[ind])$statistic
  })
}

?replicate
?sample

data("energy", package = 'ISwR')
attach(energy)
t.emp <- t.test(expend ~ stature)$statistic
t.perm <- simP(expend, stature, 1000)
t.boot <- bootP(expend, stature, 1000)

t.emp
t.perm
t.boot

mm <- range(c(t.emp, t.perm, t.boot))
mm
p <- (sum(abs(t.perm) - abs(t.emp) >= 0) + 1) / (1000 + 1)
p # pseudo - p

CI <- quantile(t.boot, probs = c(0.0025, 0.975))
CI # confidence level

plot(density(t.boot), col = 'blue', lwd = 2, xlim = c(-10, 5), 
     ylim = c(0, 0.4), main = "", 
     xlab = "", ylab = 'Denisty of probability')
x1 <- min(which(dens$x >= CI[2]))
x2 <- max(which(dens$x < CI[1]))
with(dens, 
     polygon(x = c(x[c(x1, x1:x2, x2)]), 
             y = c(0, y[x1:x2], 0), col='gray')
)
abline(v = t.emp, col = 'green', lwd =2 )
lines(density(t.perm), col = 'red', lwd = 2)
lines(density(t.boot-5), col = 'black', lwd = 2)
text(-8, 0.36, 'Area H1')
text(-3.1, 0.36, 't(obs)')
text(2, 0.36, 'Area H0')

#
power.t.test(
  delta = 3.0, 
  sd = 1.8,
  sig.level = 0.05, 
  power = 0.8
#  , type = 'paired'
  ,  type = 'one.sample'
)
power.t.test(
  n = 15, 
  delta = 3.0, 
  sd = 1.8,
  sig.level = 0.05
#  , power = 0.8
)

# alpha - уровень значимости, например 0.05, вероятность ошибка 1 рода,
# то есть вероятность отвергнуть правильную нулевую гипотезу
# betta - вероятоность ошибки 2 рода, то есть принять неверную нулевую 
# гипотезу.
# Мощность критерия = 1 - betta. 
###########################################################################

###########################################################################

# Deviance comaprison in groups
# Fisher F-test , 2 Level only of factor, no more. 
data("energy", package = 'ISwR')
attach(energy)
var.test(expend ~ stature)
View(energy)

# more than 2 factors

# Levene Test
View(InsectSprays)
#df = InsectSprays
#var.test(df$count ~ df$spray)
library(car)
leveneTest(count ~ spray, data = InsectSprays)
leveneTest(count ~ spray, data = InsectSprays, center = mean)

# Bartlet Test
bartlett.test(count ~ spray, data = InsectSprays)

# Fligner Test
fligner.test(count ~ spray, data = InsectSprays)

# Brown-Forsite
#install.packages('HH')
library(HH)
hov(count ~ spray, data = InsectSprays)
hovPlot(count ~ spray, data = InsectSprays)

hovBF(count ~ spray, data = InsectSprays)
hovplotBF(count ~ spray, data = InsectSprays)

###########################################################################

###########################################################################
# analysis of variance (ANOVA)

# example 1
tomato <- data.frame(weight = c(1.5, 1.9, 1.3, 1.5, 2.4, 1.5,
                                1.5, 1.2, 1.2, 2.1, 2.9, 1.6,
                                1.9, 1.6, 0.8, 1.15, 0.9, 1.6),
                     trt = rep(c("Water", "Nutrient", "Nutrient+24D"),
                               c(6, 6, 6)))
View(tomato)
tomato$trt <- relevel(tomato$trt, ref = "Water")
attach(tomato)
stripchart(weight ~ trt, xlab = 'Weight', ylab = 'Condition')
(Means <- tapply(weight, trt, mean))

summary(aov(weight ~ trt, data = tomato))

# example 2
#install.packages('HSAUR2')
library(HSAUR2)
data(weightgain)
str(weightgain)
library(ggplot2)
ggplot(data = weightgain, 
       aes(x = type, y = weightgain)) + 
  geom_boxplot(aes(fill = source))

Hmisc::describe(weightgain)
pastecs::stat.desc(weightgain)
library(doBy)
#install.packages('doBy')
doBy::summaryBy(weightgain ~ type + source, data = weightgain, 
                FUN = c(mean, sd, length))
plot.design(weightgain)
###########################################################################

###########################################################################
# CORRELATION ANALYSIS 

# FOR NUMERIC DATA

# if normal distribution
# Pearson
cor.test() 

# if not normal distribution
# Spearman
cor.test(method = 'spearman') 

# Kendall
cor.test(method = 'kendall') 

###########################################################################

###########################################################################

# CHI-SQUARe

#---------------------------------------------------
#   FOR INDEPENDENT SAMPLES
#---------------------------------------------------
# 2x2 table
qchisq(p = 0.95, df = 1)

mice <- matrix(c(13, 44, 25, 29), nrow = 2, byrow = TRUE)
mice
chisq.test(mice)


# more than 2x2 Table

light <- c(12, 40, 45)
dark <-  c(87, 34, 75)
very.dark <- c(3, 8, 2)

color.data <- matrix(
  c(light, dark, very.dark),
  nrow = 3,
  dimnames = list(c('Pop1', 'Pop2', 'Pop3'),
                  c('Light', 'Dark', 'Very dark')
                  )
)
color.data
chisq.test(color.data)


# less than 5 observations in cell
# Exact Fisher Test
(X <- matrix(c(1, 10, 8, 4), ncol = 2))
X
fisher.test(X)

#---------------------------------------------------
#   FOR DEPENDENT SAMPLES
#---------------------------------------------------


# McNemar test instead of Chi-Square

data <- read.table(header=TRUE, text='
 subject time result
                   1  pre      0
                   1 post      1
                   2  pre      1
                   2 post      1
                   3  pre      0
                   3 post      1
                   4  pre      1
                   4 post      0
                   5  pre      1
                   5 post      1
                   6  pre      0
                   6 post      1
                   7  pre      0
                   7 post      1
                   8  pre      0
                   8 post      1
                   9  pre      0
                   9 post      1
                   10  pre      1
                   10 post      1
                   11  pre      0
                   11 post      0
                   12  pre      1
                   12 post      1
                   13  pre      0
                   13 post      1
                   14  pre      0
                   14 post      0
                   15  pre      0
                   15 post      1
                   ')
View(data)

#install.packages('reshape2')
library(reshape2)
data.wide <- dcast(data, subject  ~ time, value.var = 'result')
View(data.wide)
ct <- table(data.wide[, c('pre', 'post')])
ct

mcnemar.test(ct)
mcnemar.test(ct, correct = FALSE) # Edwards addition


# for more than 2 times test-tables (2x2)
# K tests where 2x2 was used

# Cohran-Mantel-Haenszel test  i.e. CMH test

drug <- array(c(11, 10, 25, 27,
    16, 22, 4, 10,
    14, 7, 5, 12,
    2, 1, 14, 16,
    6, 0, 11, 12,
    1, 0, 10, 10,
    1, 1, 4, 8,
    4, 6, 2, 1),
    dim = c (2, 2, 8),
    dimnames = list(
      Group = c ("Drug", "Control"),
      Response = c("Success", "Failure"),
      Center = c("1", "2", "3", "4", "5", "6", "7", "8")
    )
)
View(drug)
drug

mantelhaen.test(drug)
mantelhaen.test(drug, correct = FALSE)

###########################################################################

###########################################################################
# POWER OF TESTS

votes <- matrix(c(28, 72, 20, 80), ncol = 2, byrow = T)
votes

chisq.test(votes)
res <- chisq.test(votes)
res


obs <- res$observed
exptd <- res$expected
obs <- obs / 200
exptd <- exptd/ 200
sqrt(sum((exptd - obs) ^ 2/ exptd))
# or :
#install.packages('pwr')
library(pwr)
ES.w2(obs)    # size/volume of effect

# Power of test:
# observed power: 
pwr.chisq.test(w = ES.w2(obs), df = 1, N = 200)


# how to define sample size needed:

pwr.chisq.test(
  w = 0.15, 
  N = NULL,
  df = 1,
  sig.level = 0.05,
  power = 0.8
)

?pwr
###############################################################################



