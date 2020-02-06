setwd('~/src/nicar_logistic_regression/')

# Use the readr library to read in the .csv data
library(readr)
library(tidyr)
data <- read_csv('data/boston.csv')
str(data)
summary(data)

data <- read_csv('data/boston.csv') %>%
  select(ticket,day,mph,zone,mphover,mphpct,age,minority,female) %>%
  drop_na

# For convenience, remove fields we don't need
data <- data[c('ticket','day','mph','zone','mphover','mphpct','age','minority','female')]
str(data)

# Missing data
is.na(data) # gives a matric of TRUE or FALSE
# apply the which function by column to the TRUE/FALSE matrix
apply(is.na(data), 2, which)
# Values for minority and female variable are missing in row 87
data[87,]
# Delete rows with NAs
data <- data[!(is.na(data$minority) | is.na(data$female)),]
nrow(data) # one row deleted

# Crosstabs to make sure we have enough cases in each category
xtabs(~ticket, data=data) # Uneven distribution = biased results
xtabs(~ ticket+day, data=data)
xtabs(~ ticket+mph, data=data)
xtabs(~ ticket+zone, data=data)
xtabs(~ ticket+mphover, data=data)
xtabs(~ ticket+mphpct, data=data)
xtabs(~ ticket+age, data=data)
xtabs(~ ticket+minority, data=data)
xtabs(~ ticket+female, data=data)

# We can use binary explanatory variables in linear regression.
# Can we also use linear regression when the response variable?
plot(data$mphpct, data$ticket)
m.linear <- lm(ticket ~ mphpct, data=data)
abline(m.linear, col='green')

# Values on the regression line are the estimated mean of 
# x for a given value of y or sum(x)/n. Since x is 0 or 1,
# that equals tickets/number of stops or the probability of
# getting a ticket given the percentage above the speed limit
# you were driving. But:

summary(m.linear)
# x = -0.326162 + 0.019729 * y
-0.326162 + 0.019729 * 70 # probability of 1.05 ???

# Probablity: success / trials
# Odds: success / failures
# O = p / (1-p)
# p = O / (O + 1)
# Probablility of getting heads given 50 heads in 100 tosses:
# 50 / 100 = 0.5 (50%)
# Odds of getting heads given 50 heads in 100 tosses:
# 50 / (100 - 50) = 1 (to 1)
# Odds can be any value between 0 and infinity
# log(Odds) can be any values from -infinity to +infinity

# Logit or log(Odds)) function
# p(y) = log(A + Bx) / (log(A + Bx) + 1)
# p(y) / (1 - p(y)) = log(A + Bx)
# Odds(y) = log(A + Bx)
# log(Odds(y)) = A + Bx
# GLM: Generalized Linear Model

m.logistic <- glm(ticket ~ mphpct, data=data, family='binomial')
plotData <- data.frame(mphpct=seq(min(data$mphpct),max(data$mphpct), len=100))
plotData$ticket = predict(m.logistic, newdata=plotData, type='response')
lines(ticket~mphpct, plotData, col='red')
summary(m.logistic)

m.minority <- glm(ticket ~ minority, data=data, family='binomial')
summary(m.minority)

# McFadden's psuedo R-squared = (Null deviance - Residual deviance) / Null deviance
(149.99 - 135.85) / 149.99

exp(coef(m.minority))

# What is the null model: the simplest possible model -- not very predictive
m.null <- glm(ticket ~ 1, data=data, family='binomial')
summary(m.null)
O = exp(coef(m.null))
p = O / (O + 1) # 0.4495413
mean(data$ticket) # 0.4495413
(m.null$null.deviance - m.null$deviance) / m.null$null.deviance

m.full <- glm(ticket ~ ., data = data, family=binomial)
summary(m.full)

ticket <- data %>%
  group_by(ticket) %>%
  summarise(freq=n(),prob=(n()/nrow(.)),odds=prob/(1-prob), logodds=log(odds)) %>%
  round(., 5)
