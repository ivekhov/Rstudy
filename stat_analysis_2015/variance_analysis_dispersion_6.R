# EXPLORATORY DATA ANALYSIS. Process




#-------------------------------------------------------------------
# Collinearity analysis
#-------------------------------------------------------------------
load(file = 'sleep_imp.Rdata')
M <- cor(sleep_imp3)
library(corrplot)
col4 <- colorRampPalette( c( "#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F", "cyan", "#007FFF", "blue", "#00007F"))
corrplot(M, method = 'color', col = col4(20), cl.length = 21, 
         order = 'AOE', addCoef.col = 'green')

library(car)

vif(
  lm(
    Sleep ~ BodyWgt + BrainWgt + Span + Gest + Pred + Exp + Danger,
    data = sleep_imp3
  )
)


#-------------------------------------------------------------------
# Correlation among variables
#-------------------------------------------------------------------

cars <- mtcars[, 1:7]
pairs(cars, panel = panel.smooth)
#

# manual chart creation

panel.cor <- function(x, y, digits  = 2, 
                      prefix = '', cex.cor, ..)
{
  usr <- par('usr'); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method = 'spearman'))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = '')
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
  
}
pairs(cars, panel = panel.smooth, lower.panel = panel.cor)
#

# chart from lattice

library(lattice)
splom(cars)
#

# Ggally

library(GGally)
ggpairs(cars,
        upper = list(continuous = 'density', combo = 'box'),
        lower = list(continuous = 'points', combo = 'dot')
)
#

# EDF effective degree of freedom
# nonlinear relation (if > 1.5 means that non-linear connection exists)

library(mgcv)
summary(gam(mpg ~ s(hp) + s(qsec), data = cars))
#

#-------------------------------------------------------------------
# Realtionship among predictors
#-------------------------------------------------------------------

# Categorical variables

Sparrows <- read.table(file = "SparrowsElphick.txt" , header = TRUE)
View(Sparrows)

I1 <- Sparrows$SpeciesCode == 1 &
  Sparrows$Sex != '0' &
  Sparrows$wingcrd < 65
Wing1 <- Sparrows$wingcrd[I1]
Weil <- Sparrows$wt[I1]
Mon1 <- factor(Sparrows$Month[I1])
Sex1 <- factor(Sparrows$Sex[I1])

fMonth1 <- factor(Mon1, levels = c(5, 6, 7, 8, 9), 
                  labels = c('May', 'June', 'July', 'August', 'Sep' ))
fSex1 <- factor(Sex1, levels = c(4,5 ), 
                labels = c('M', 'W'))

coplot(Weil ~ Wing1 | fMonth1 * fSex1, 
        ylab = c('Weight', 'Sex'),
        xlab = c('Length', 'Month'),
        panel = function(x, y, ...) {
          tmp <- lm(y ~ x, na.action = na.omit)
          abline(tmp)
          points(x, y)
        }
)
#

df <- data.frame(weight = Weil, length = Wing1, 
                 sex = fSex1, month = fMonth1)
df1 <- df[df$month != 'May' & df$month != 'Sep', ]

require(stargazer)
stargazer(M1, type = 'html', out = 'M1.doc')
stargazer(DT, type = 'html', out = 'DT.doc', summary = FALSE)


#-------------------------------------------------------------------
# Space and time factors  -- on dependent vatiables
#-------------------------------------------------------------------

Waders <- read.table(file = 'wader.txt',  header = TRUE)
Time <- seq(1, 25)

par(
  mfrow = c(2, 2), 
  mar = c(5, 4, 3, 2)
)
plot(Time, Waders$C.fuscicolis, type = 'l', xlab = 'Time 2 weeks', 
     ylab = 'Population C.fuscicolis')
acf(Waders$C.fuscicolis, main = 'ACF C.fuscicolis')

plot(Time, Waders$L.dominicanus, type = 'l', 
     xlab = 'Time 2 weeks', 
     ylab = 'Population L.dominicanus')
acf(Waders$L.dominicanus, main = 'ACF L.dominicanus')

# nlme::Variogram()
# geor::variog()
# gstat::variogram()

######################################################################
# p.219 Analysis fo VARIANCE 

# Aim of Variance analysis - estimate, if numerical values (means)
# are affected by other factors. Factors are nominative scale data. 


#-------------------------------------------------------------------------------
# EXAMPLE 1

tomato <- data.frame(weight = c(1.5, 1.9, 1.3, 1.5, 2.4, 1.5,
                                1.5, 1.2, 1.2, 2.1, 2.9, 1.6,
                                1.9, 1.6, 0.8, 1.15, 0.9, 1.6),
                     trt = rep(c("Water", "Nutrient", "Nutrient+24D"),
                               c(6, 6, 6)))
View(tomato)
tomato$trt <- as.factor(tomato$trt)

tomato$trt <- relevel(tomato$trt, ref = 'Water')
levels(tomato$trt)
# ?relevel
#

# LINEAR MODEL 
M <- lm(weight ~ trt, data = tomato)
summary(M)

tapply(tomato$weight, tomato$trt, mean)
#tapply(tomato$weight, mean) # error

anova(M)
aov(weight ~ trt, data = tomato)
#?aov
model.matrix(M)
#

#-------------------------------------------------------------------------------
# EXAMPLE 2

# null-hypot : average among 6 groups do not different 
# 6 groups per 12 obsercations
M <- lm(count ~ spray, data = InsectSprays)
View(InsectSprays)
str(M)
M.res <- M$residuals
M.fit <- M$fitted.values

# test of heterosked-ty on chart
plot(M.fit, M.res, pch = 19, col = 4, 
     xlab = 'predicted', ylab = 'residuals')

# test of correlation 
cor.test(fitted(M), InsectSprays$count)
#

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# MODEL ADEQUACY 

# model assumption testing
# p-value < 2.2e-16 --> null decline, MEANs differs significantly

# Condition on trusting F as correct estimation : 
#   1 target var is independent variable
#   2 target var in groups are normally distributed
#   3 in groups variance not differ (i.e. homoskeda-ty of variance)
#-------------------------------------------------------------------------------

#   1 target var is independent variable
#       (under control on experiment planning stage)
#-------------------------------------------------------------------------------

#   2 target var in groups are normally distributed
#     (overall normal distribution is not necessary: 
#       in-groups is more important) 

# charts : qqplot. General idea - testing residuals, if they are normal.
car::qqPlot(
  resid(M), 
  dist = 'norm', 
       col = palette()[1], pch = 19, 
       xlab = 'normal', 
       ylab = 'observed', 
       main = 'quantiles comparison')

# ideal normal
library(sm)
sm.density(resid(M), 
           model = 'Normal', 
           xlab = 'Imitated sample', 
           ylab = 'Function density distribution')
# --------------------------------

# distributions of variables inside groups
ggplot(InsectSprays, aes(x = count)) + geom_histogram() +
        facet_wrap(~spray) +
        xlab("Число насекомых") + ylab("Частота")

# predicted vs observed
ggplot(InsectSprays, aes(sample = count)) + 
  stat_qq() +
        facet_wrap(~spray, scales = "free_y") +
        xlab("Ожидаемые квантили") + ylab("Наблюдаемые значения")

#install.packages('gridExtra')
library(gridExtra)
InsectSprays$resids = resid(M)
library(ggplot2)


# combo chart : 1) distribution of residuals and  2) QQ-plot
p3 = ggplot(InsectSprays, aes(x = resids)) + 
        geom_histogram(aes(y=..density..)) +
        geom_density(color = "red") + 
        xlab("Остатки") + ylab("Плотность вероятности")
p4 = ggplot(InsectSprays, aes(sample = resids)) + stat_qq() +
        xlab("Ожидаемые квантили") + ylab("Наблюдаемые значения")
grid.arrange(p3, p4, ncol = 2)

# --------------------------------
# formal test: shapiro-uilka, anderson-darling, bartlett, etc. 
# on small samples - problem

shapiro.test(resid(M))
# p-value = 0.02226 : null-hypo('not differes from normality') declined --> 
# distribution is not normal. 


#-------------------------------------------------------------------------------

#   3 in groups variance not differ (i.e. homoskeda-ty of variance)
# General idea: Variance of residuals should be homogen. ('the same level')

# boxplot()
set.seed(202)
ggplot(InsectSprays, aes(x = spray, y = count)) + geom_boxplot() +
        geom_jitter(alpha = 0.5) +
        xlab("Инсектицид") + ylab("Число выживших насекомых")


# test of heterosked-ty on chart
plot(M.fit, M.res, pch = 19, col = 4, 
     xlab = 'predicted', ylab = 'residuals')

# or in ggplot : 
InsectSprays$fit = fitted(M)
ggplot(InsectSprays, aes(x = fit, y = resids)) + geom_point() +
        xlab("Предсказанные значения") + ylab("Остатки")

#по мере увеличения предсказанных моделью
# средних групповых значений разброс остатков также увеличивается. Такая «во-
# ронкообразная» форма распределения остатков типична для случаев, когда усло-
# вие однородности групповых дисперсий не выполняетс
# (growth of mean eith growth of variance == variance are not homogen)

# formal test
car::leveneTest(InsectSprays$count, InsectSprays$spray)
# p-val : 0.004223 =- > null hypo ("variance are homogen") is declined


# RESUME : one-way variance analysis could not be used here.

#-------------------------------------------------------------------------------
#   what to do?

# 1 distr normal, variance are heterosked
# Wlch test
oneway.test()
?oneway.test

# or permutation test
?lmPerm


# 2 distr notnormal, variance are homogen.   Options: 

#   1 logarithm transformation 

#   2 degree transformation Box-Cox

#   3 non-parametric analysis: rang variance analysis Kruskell-Walless


# 3 distr notnormal, variance are heterosked. options -->> 
#   1 GLM. 
#     log transfomation -->> new model 

#   2 non-param methods (f.e. Kruskell-Walless)

#-------------------------------------------------------------------------------
# EXAMPLE 3 scenario: 

M.log <- lm(log(count + 1) ~ spray , data = InsectSprays)
summary(M.log)

shapiro.test(resid(M.log))
# normality test is passed: p-value = 0.5348

car::leveneTest(log(InsectSprays$count + 1), InsectSprays$spray)
# homogen of Variance test is passed : p = 0.1093
################################################################################

################################################################################
# KRUSKELL-WALLES variance analysis
################################################################################

# non-parametrical method, normal distribution is not  necessary
# null--hypo: no difference among groups 
#  with 2 groups KRUSKELL-WALLES == MANN-UITNI

#-------------------------------------------------------------------------------
# EXAMPLE 

kruskal.test(count ~ spray, data = InsectSprays)
# p-value = 1.511e-10 --> hull hypo is declined == differenece exists
################################################################################


################################################################################
# TWO- and MANY-FACTORS variance analysis
################################################################################

# aov problem - data should be balanced in groups evaluated

# 2way ANOVA : impacat of 2 factors ad relation between 2 factors

data(weightgain, package="HSAUR2")
M2 <- lm(weightgain ~ type * source, data = weightgain)
summary(M2)

# Aim of Variance analysis - estimate, if numerical values (means)
# are affected by other factors. Factors are nominative scale data. 

summary(M2)
anova(M2)
#

M3 <- lm(weightgain ~ source * type, data = weightgain)
anova(M3)
#

# illustration of how disbalanced samples affects results
weightgain2 <- weightgain[-c(1:6, 34:40), ]
M4 <- lm(weightgain ~ type * source, data = weightgain2)
M5 <- lm(weightgain ~ source * type, data = weightgain2)

anova(M4)
anova(M5)

summary(M4)
summary(M5)

plot.design(weightgain2)
#
# MANY-FACTORS anova (3, 4, and more)
# quite complicated 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

################################################################################
# CONTRASTS in LINEAR MODELS with CATEGORICAL PREDICTORS
################################################################################

boxplot(count ~ spray, data  = InsectSprays, col = 'coral')
contrasts(InsectSprays$spray)

# usage : grouped average analysis of sub-groups
# f.e. : three first group on average differ from average last three sub-groups

con1 <- c(1,1,1, -1, -1, -1)
con2 <- c(1, 1, -1, -1, -1, 1)

con.matrix <- cbind(con1, con2)

contrasts(InsectSprays$spray) <- con.matrix
M6 <- aov(count ~ spray, data = InsectSprays)
summary(M6, split = list(
  spray = list("First 3 against others" = 1, 
               "ABF against CDE" = 2)
))
# conclusion : difference in both cases exist.
#-------------------------------------------------------------------------------


################################################################################
# MULTIPLE TESTING OF STATISTICAL HYPOTHESIS 
################################################################################

# General idea - minimize share of false errors 1 and 2 type
# among  all tests on tested sample. 

# Methods on control error level in multiple tests. 

#-------------------------------------------------------------------------------

# BONFERRONI correctiion 
# hardcore

p.adjust(c(0.01, 0.02, 0.005), method = 'bonferroni')
alpha <- 0.05
p.adjust(c(0.01, 0.02, 0.005), method = 'bonferroni') < alpha
#-------------------------------------------------------------------------------

# HOLMs correction
# more power than bnfrni

p.adjust(c(0.01, 0.02, 0.005), method = 'holm')
alpha <- 0.05
p.adjust(c(0.01, 0.02, 0.005), method = 'holm') < alpha
#-------------------------------------------------------------------------------

# PROBLEM: 
# hardcore test -->> a lot null-hypo didn`t rejected, but should be regected

#-------------------------------------------------------------------------------

# BENJAMINI and HOCHBERG method
# FDR -- False Discovery Rate : % false among all rejected

pvals <-c(0.0001, 0.0004, 0.0019, 0.0095, 0.0201,
  0.0278, 0.0298, 0.0344, 0.0459, 0.3240,
  0.4262, 0.5719, 0.6528, 0.7590, 1.000)
p.adjust(pvals, method = 'BH')
#-------------------------------------------------------------------------------

# Benjamini-Yekutieli

p.adjust(pvals, method = 'BY')
#-------------------------------------------------------------------------------


################################################################################
# APOSTERIORI comparison of groups means
################################################################################

# After rejecting null-hypo (if p < 0.05):
# aposteroro analysis : 
# which groups are different significantly from each other?

# Range of criteria for estimating  this 

#-------------------------------------------------------------------------------
# 
# Tukey  : 
# pair comparisons
# condition : normal distr of data, homogenirt of variance

waterbodies <- data.frame(Water = rep(c("Grayson", "Beaver",
                                        "Angler", "Appletree",
                                        "Rock"), each = 6),
                          Sr = c(28.2, 33.2, 36.4, 34.6, 29.1, 31.0,
                                 39.6, 40.8, 37.9, 37.1, 43.6, 42.4,
                                 46.3, 42.1, 43.5, 48.8, 43.7, 40.1,
                                 41.0, 44.1, 46.4, 40.2, 38.6, 36.3,
                                 56.3, 54.1, 59.4, 62.7, 60.0, 57.3) )

M <- aov( Sr ~ Water, data = waterbodies)
summary(M)
TukeyHSD(M)

par(mar = c(4.5, 8, 4.5, 1.5))
plot(TukeyHSD(M), las = 1)
#-------------------------------------------------------------------------------

waterbodies$Water <- as.factor(waterbodies$Water)
Mlm <- lm( Sr ~ Water, data = waterbodies)
summary(Mlm)

# linear model goes from condition that independent vars are lineary independent
#-------------------------------------------------------------------------------


# multcomp package
install.packages('multcomp')
library(multcomp)

coef(Mlm)
vcov(Mlm)

glht(Mlm, linfct = mcp(Water = "Tukey"))
glht(Mlm, linfct = mcp(Water = c(
        "Rock - Angler = 0",
        "Grayson - Appletree = 0",
        "Grayson - Beaver = 0"))
)
?glht

# conttrasts pre-scripted 
contr <- rbind("Rock - Angler" = c(-1, 0, 0, 0, 1),
               "Grayson - Appletree" = c(0, -1, 0, 1, 0),
               "Grayson - Beaver" = c(0, 0, -1, 1, 0) )

glht(Mlm, linfct = mcp(Water = contr))
summary(glht(Mlm, linfc = mcp(Water = "Tukey")))

# confidence levels 
mult <- glht(Mlm, linfct = mcp(Water = contr))
confint(mult, level = 0.95)
plot(confint(mult, level = 0.95))
#################################################################
