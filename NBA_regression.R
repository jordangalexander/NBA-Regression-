################################
#   NBA Regression Project     #
################################

install.packages("devtools") #if needed...
devtools::install_github("MichaelJMahometa/SDSRegressionR")

library(SDSRegressionR)
library(multcomp)
library(nnet)

# Data Initialization
teams <- read.csv("NBA_regseason.csv", stringsAsFactors = FALSE)
teams <- as.data.frame(teams)

initialModel <- lm(PTS ~ DREB + TOV + PER_cat, data = teams)
summary(initialModel)

# Data Visualization/Outlier Testing
simpleScatter(teams, TOV, PTS, line = TRUE)
residFitted(initialModel)
cooksPlot(initialModel, save.cutoff = TRUE)
threeOuts(initialModel)
confint(initialModel)

# Dummy Coding
teams$PER_1 <- NA
teams$PER_1[!is.na(teams$PER_cat) ] <- 0
teams$PER_1[teams$PER_cat == 1] <- 1

teams$PER_2 <- NA
teams$PER_2[!is.na(teams$PER_cat) ] <- 0
teams$PER_2[teams$PER_cat == 2] <- 2

teams$PER_3 <- NA
teams$PER_3[!is.na(teams$PER_cat) ] <- 0
teams$PER_3[teams$PER_cat == 3] <- 3

teams$PER_4 <- NA
teams$PER_4[!is.na(teams$PER_cat)] <- 0
teams$PER_4[teams$PER_cat == 4] <- 4

# New Model Run
catModel <- lm(PTS ~ DREB + TOV + PER_1 + PER_2 + PER_3, data = teams)
summary(catModel)
lmBeta(catModel)
lmDecomp(catModel, "PER_cat", "PTS",  mod.type=2, print.ros = TRUE)

# Evaluate the PER dummy variables
mod1 <- lm(PTS ~ DREB + TOV, data = teams)
mod2 <- lm(PTS ~ DREB + TOV + PER_1 + PER_2 + PER_3, data = teams)
anova(mod1, mod2)
summary(mod2)$r.squared - summary(mod1)$r.squared

# Post-Hoc Exploration
checkA <- lm(PTS ~ DREB + TOV + PER_1, data = teams)
jcomp <- summary(glht(checkA, linfct = c("PER_1 = 0")))
summary(jcomp, test = adjusted("holm"))

checkB <- lm(PTS ~ DREB + TOV + PER_1 + PER_2 + PER_3, data = teams)
jcomp <- summary(glht(checkB, linfct = c("PER_1 = 0", "PER_2 = 0", "PER_3 = 0")))
summary(jcomp, test = adjusted("holm"))

# Center and Spread
mean(teams$DREB)
sd(teams$DREB)

mean(teams$TOV)
sd(teams$TOV)

mean(teams$PER)
sd(teams$PER)

table(teams$PER_cat)

mean(teams$PTS)
sd(teams$PTS)


x <- multinom(Team ~ PTS + REB + TOV + PER_cat, data = teams)
summary(x)
