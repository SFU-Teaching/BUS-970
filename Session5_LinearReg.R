############################################################
# BUS 961 | Fall 2022 | Session 5
# LINEAR REGRESSIONS - Single and Multiple
# CK 
# 12 Oct 2022

### REFERENCES
# Regression Diagnostics https://www.andrew.cmu.edu/user/achoulde/94842/homework/regression_diagnostics.html
############################################################


#### PREAMBLE : ## Clearing mem buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()
set.seed(42) # Set a seed to ensure repeatable random samples

# libraries
require(data.table)
require(pastecs)
require(stargazer)


#################################################
## SIMPLE SINGLE LINEAR MODEL
#################################################

## http://r-statistics.co/Linear-Regression.html 

?cars

# we start with some basic analysis and graphical analysis

stat.desc(cars) # summary desc stats

scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")  
# scatterplot
library(ggplot2)
ggplot(data=cars,aes(x=speed,y=dist)) + 
  geom_point() + 
  geom_smooth(formula = y~x, method=lm)

# =>  suggestive of linear relationship

# check for outliers
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed")  # box plot for 'speed'
boxplot(cars$dist, main="Distance")  # box plot for 'distance'
#Generally, any datapoint that lies outside the 1.5 * interquartile-range  is considered an outlier, 
#where, IQR is calculated as the distance between the 25th percentile and 75th percentile values 
# => one outlier in distance

# check for normality 
par(mfrow=c(1, 2))  # divide graph area in 2 columns
# density plot for 'speed'
hist(cars$speed, freq = FALSE,breaks = 10, col='red') +
  lines(density(cars$speed))
# density plot for 'dist'
hist(cars$dist, freq = FALSE,breaks = 10, col = "blue") + lines(density(cars$dist))
#if not normal - we need to transform
#TODO: we'll get to transformation later
par(mfrow=c(1,1))

#Correlation
cor(cars$dist,cars$speed)


# linear model
Model1 <- lm(dist~speed,data=cars) # we trying to figure out stopping distance y as function of x
summary(Model1)
# => dist = -17.579 + 3.932*speed

# see p value:
# remember in OLS:  Null Hypothesis is that the coefficients associated with the variables is equal to zero
# alt h1:  coefficients are not equal to zero => statistically significant relationship between the y and x
# small p value => reject null and accept h1

# t value:
# heuristic: larger t value => less likely coeffecient \beta neq 0 by chance
# Pr(>|t|) or p-value is the probability that you get a t-value as high or higher than the observed value 
# when the Null Hypothesis (the \beta coefficient is equal to zero or that there is no relationship) is true.
# So if the Pr(>|t|) is low, the coefficients are significant (significantly different from zero). 
# If the Pr(>|t|) is high, the coefficients are not significant.

# if you want a different significance level, just see the p value and signif codes. 

# NOTE on AIC AND BIC
# The Akaikes information criterion - AIC (Akaike, 1974) and the Bayesian information criterion - BIC (Schwarz, 1978) 
#measures of the goodness of fit of an estimated statistical model and can also be used for model selection. 
# Both criteria depend on the maximized value of the likelihood function L for the estimated model.
AIC(Model1)
BIC(Model1)


# MODEL DIAGNOSTICS:
# Regression Diagnostics https://www.andrew.cmu.edu/user/achoulde/94842/homework/regression_diagnostics.html

#1. Residuals vs. Fitted
ggplot(data=Model1,aes(x=Model1$fitted.values,y=Model1$residuals)) +
  geom_point() + geom_smooth(formula = y~x, method=lm)

#2. Normality of Residuals
hist(Model1$residuals, freq = FALSE,breaks = 10, col='green') + lines(density(Model1$residuals))
#2B Q-Q plot
qqnorm(Model1$residuals, pch = 1, frame = FALSE) 
qqline(Model1$residuals, col = "steelblue", lwd = 2)

# R Shortcut
plot(Model1)

par(mfrow = c(2, 2)) # r outputs 4 graphs - this puts them in 1 plot
plot(Model1, main = "Model 1")

# recall 
par(mfrow = c(1, 1)) # remember to change it back!

# Neat Publication Style outputs
# Ref: https://www.jakeruss.com/cheatsheets/stargazer/
library(stargazer)

#Summ Stats
stargazer(cars,type="text")

# Reg Output:
stargazer(Model1, type = "text")


## PLAYING AROUND
# Let's do a model without the intercept
Model2 <- lm(dist~speed-1,data=cars)
summary(Model2)

# Reg Output:
stargazer(Model1,Model2, type = "text", style="all")

# Whats happening?
# Intercept - yes or no?
# -> IT DEPENDS ON THE MODEL
# -> https://stats.stackexchange.com/questions/7948/when-is-it-ok-to-remove-the-intercept-in-a-linear-regression-model 
#-> The shortest answer: never, unless you are sure that your linear approximation of the data generating 
# process (linear regression model) either by some theoretical or any other reasons 
# is forced to go through the origin.
## If not the other regression parameters will be biased even if intercept is statistically insignificant 


#Refer: https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-why-are-r2-and-f-so-large-for-models-without-a-constant/


#################################################
## Regressing AQ DAta - SEssion 5 ###########
#################################################7
# refer: https://rstudio-pubs-static.s3.amazonaws.com/52381_36ec82827e4b476fb968d9143aec7c4f.html 

aq1 <- fread("C:/Users/CK/Dropbox/SFU TEACHING/PhD BUS  961 Quant Methods/961 SLIDES and Notes/Data/AQ_1.csv")
aq2 <- fread("C:/Users/CK/Dropbox/SFU TEACHING/PhD BUS  961 Quant Methods/961 SLIDES and Notes/Data/AQ_2.csv")
aq3 <- fread("C:/Users/CK/Dropbox/SFU TEACHING/PhD BUS  961 Quant Methods/961 SLIDES and Notes/Data/AQ_3.csv")
aq4 <- fread("C:/Users/CK/Dropbox/SFU TEACHING/PhD BUS  961 Quant Methods/961 SLIDES and Notes/Data/AQ_4.csv")

# let's build 4 models
Model1 <- lm(aq1$y~aq1$x) # aq 1
Model2 <- lm(aq2$y~aq2$x) # aq 2
Model3 <- lm(aq3$y~aq3$x) # aq3
Model4 <- lm(aq4$y~aq4$x) #aq 4

# sumamaries of Model
summary(Model1)
summary(Model2)
summary(Model3)
summary(Model4)

# see RSE, RSquared, Adj R., and F-stat!
# everything looks the same! WTF!

# Notice residuals are different.

# let's do diagnostics

#1. first look at scatter plots!
library(ggplot2) # let's use ggplot!
# ggplot is awesome!

# ggplot does not work with par(mfrow=c(2,2))
ggplot(data=aq1,aes(x=x,y=y)) + geom_point() + geom_smooth(formula = y~x, method=lm)
ggplot(data=aq2,aes(x=x,y=y)) + geom_point() + geom_smooth(formula = y~x, method=lm)
ggplot(data=aq3,aes(x=x,y=y)) + geom_point() + geom_smooth(formula = y~x, method=lm)
ggplot(data=aq4,aes(x=x,y=y)) + geom_point() + geom_smooth(formula = y~x, method=lm)
# Outliers are an issue

#2.  Let's do model diagnostics and see residuals
par(mfrow = c(2, 2))
plot(Model1, main = "Model 1")
plot(Model2, main = "Model 2")
plot(Model3, main = "Model 3")
plot(Model4, main = "Model 4")

# look at residuals vs. Fitted and normal q-q plots!

# what is the best model?!

# dataset 1!
#See  https://towardsdatascience.com/importance-of-data-visualization-anscombes-quartet-way-a325148b9fd2 


#################################################
## Multiple LINEAR MODEL
#################################################

# Let's use a regular dataset
library(datasets)
?mtcars

dt <- mtcars

#summary stats - using different packages
summary(dt) #base
stat.desc(dt) #pastecs
stargazer(dt,type="text") #stargazer

# let's look at correlations 
# refer: http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram

round(cor(dt),2) # round to 2 digits

# Sexier plots
# refer2 : http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software 

# with statistical significance
library("Hmisc")
rcorr(as.matrix(dt))
# correlations in table 1
# p values in table 2

##install.packages("PerformanceAnalytics")
#library(PerformanceAnalytics)
#chart.Correlation(dt, histogram=TRUE, pch=19) # get's busy
## to make this unbusy, use only variables in your interest.
## MPG is correlated on everything!
#chart.Correlation(dt[,c(1,3,4,5,6,7)], histogram=TRUE, pch=19) # shows statistical correlations


# okay, let's get back to regressions
?mtcars

#kitchen Sink model -- when you run a reg on everythin

Model.KS <- lm(mpg~.,data=dt)
summary(Model.KS)
stargazer(Model.KS, type = "text")

# let's try to model mpg as a function of transmission (am), weight (wt), displacement (disp)

Model1 <- lm(mpg~am,data=dt)
summary(Model1)
Model2 <- lm(mpg~am+wt,data=dt)
summary(Model2)
Model3 <- lm(mpg~am+wt+hp+disp,data=dt)
summary(Model3)
Model4 <- lm(mpg~am+wt+hp+disp+cyl,data=dt)
summary(Model4)

stargazer(Model1,Model2,Model3,Model4, type = "text")
# look at adjusted R-squared

stargazer(Model.KS,Model4,type = "text")


# MODEL DIAGNOSTICS:
# Regression Diagnostics https://www.andrew.cmu.edu/user/achoulde/94842/homework/regression_diagnostics.html

par(mfrow = c(2, 2)) # r outputs 4 graphs - this puts them in 1 plot
plot(Model2, main = "Model 2")

par(mfrow = c(2, 2)) # r outputs 4 graphs - this puts them in 1 plot
plot(Model4, main = "Model4")

par(mfrow = c(2, 2)) # r outputs 4 graphs - this puts them in 1 plot
plot(Model.KS, main = "Model.KS")

# INTERACTIONS
par(mfrow=c(1,1))
plot(dt$cyl,dt$disp)
scatter.smooth(x=dt$disp, y=dt$cyl, main="Dist ~ Speed") 
cor(dt$disp,dt$cyl)
# notice disp and cyl are highly correlated - is there an interaction?
dt$cylxdisp <- dt$cyl*dt$disp
Model4A <- lm(mpg~am+wt+hp+disp+cyl+cylxdisp,data=dt)
summary(Model4A)
stargazer(Model4,Model4A,type = "text")
# did the model improve?

#TAKEAWAY: Modeling is an art-form. Let theory drive your models.