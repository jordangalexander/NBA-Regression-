library(SDSRegressionR)
library(data.world)
library(tidyverse)
library(MASS)
library(ISLR)
library(dplyr)
library(SDSRegressionR)
library(shiny)
library(ggplot2)
library(modelr)
library(rms)
library(class)
require(ISLR)
library(leaps)

#Bring in data
cavs <- read.csv("CAVS16-17.csv", stringsAsFactors = FALSE)
names(cavs)

# logical indexing for logistic regression
cavs$wL <- NA
cavs$wL [cavs$W.L == "W"] <- 1
cavs$wL [cavs$W.L == "L"] <- 0

# get training/testing data
trainingData = subset(cavs, PF > 18)
testData = subset(cavs, PF <= 18)

# best subset selection algorithm
regfit.full = regsubsets(wL ~ .-MATCHUP-W.L, data = cavs, nvmax = 10, method = "forward")
summary(regfit.full) 

glm.fit <- glm(wL ~ MIN + PTS + FGA + FTA + REB + STL + TOV, data = cavs, family = "binomial")
summary(glm.fit)

# odds ratios
exp(glm.fit$coef)
exp(confint.default(glm.fit)) # confidence intervals

# predictive modeling with logistic regression
glm.probs=predict(glm.fit, type = "response") 
glm.probs[1:5]
glm.pred=ifelse(glm.probs > .6, 1, 0)
mean(glm.probs)

table(glm.pred, cavs$wL) # confusion matrix
mean(glm.pred == cavs$wL) # percentage correctly predicted
mean(glm.pred != cavs$wL) # percentage incorrectly predicted

# linear discriminant analysis model
lda.fit <- lda(wL ~ MIN + PTS + FGA + FTA + REB + STL + TOV, data = trainingData, family = "binomial")
lda.fit

# predictive modeling 
lda.pred = predict(lda.fit, testData)
table(lda.pred$class, testData$studytime)
mean(lda.pred$class == testData$studytime)

