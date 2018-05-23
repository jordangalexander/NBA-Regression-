#### Logisgtic Regression for the Dallas Mavericks ####
#### 2016-2017 Regular Season Data Used ####

library(SDSRegressionR)

#Bring in data
mavs <- read.csv("MAVS.csv", stringsAsFactors = FALSE)
names(mavs)

#Intital Model
mod1 <- glm(W.L ~ AST + FTA + PTS, data=mavs, family="binomial")
summary(mod1)

library(car)
vif(mod1)
cooksPlot(mod1, save.cutoff = TRUE, print.obs = TRUE, sort.obs = TRUE)
threeOuts(mod1)

#Get good data...
g_mod1 <- mavs[!row.names(mavs) %in% c(21, 140, 11, 159),]

#Re-run
mavs2 <- glm(LOW ~ AGE + LWT + SMOKE, data=g_mavs, family="binomial")
summary(mavs2)

#Odds-ratios
exp(mavs2$coef)
exp(confint.default(mavs2))

#Stats
library(rms)
mavs2.2 <- lrm(LOW ~ AGE + LWT + SMOKE, g_mavs)
mavs2.2

#Examine the variables of interest graphically...
#Look at ranges...
summary(g_mavs)

#Predict
new <- data.frame(AGE = mean(g_mavs$AGE, na.rm=TRUE),
                  LWT = seq(80, 250, 10),
                  SMOKE = mean(g_mavs$SMOKE, na.rm=TRUE))
pred <- data.frame(new, pred = predict(mavs2, newdata = new, type="link", se.fit=TRUE))
pred <- data.frame(pred, p.fit = plogis(pred$pred.fit), 
                   LL = plogis(pred$pred.fit - (1.96*pred$pred.se.fit)),
                   UL = plogis(pred$pred.fit + (1.96*pred$pred.se.fit)))

#Graph
g <- simpleScatter(g_mavs, LWT, LOW, title="Low birth weight", xlab="Mother's weight", ylab="Low birth weight probability")
g + 
  geom_line(data=pred, aes(x=LWT, y=p.fit), color="red") + 
  geom_line(data=pred, aes(x=LWT, y=LL), linetype="dashed") +
  geom_line(data=pred, aes(x=LWT, y=UL), linetype="dashed")

summary(mavs2)

#Out of curiosity...
new <- data.frame(AGE = mean(g_mavs$AGE, na.rm=TRUE),
                  LWT = mean(g_mavs$LWT, na.rm=TRUE),
                  SMOKE = c(0,1))
pred <- data.frame(new, pred = predict(mavs2, newdata = new, type="link", se.fit=TRUE))
pred <- data.frame(pred, p.fit = plogis(pred$pred.fit), 
                   LL = plogis(pred$pred.fit - (1.96*pred$pred.se.fit)),
                   UL = plogis(pred$pred.fit + (1.96*pred$pred.se.fit)))
pred

ggplot(pred, aes(y=p.fit, x=factor(SMOKE))) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=LL, ymax=UL), width=.1) +
  ylim(0,1) +
  labs(title="Smoking and low birth \n 95% CI") +
  geom_hline(yintercept = 0.5, color="red") + 
  theme_bw()

