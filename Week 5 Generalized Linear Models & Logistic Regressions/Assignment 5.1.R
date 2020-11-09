library(survival)
data(rats)
head(rats)

fitB1 <- glm(cbind(status, 1-status) ~ rx, family = binomial, data = rats)
fitB1
summary(fitB1)$coef
sqrt(diag(summary(fitB1)$cov.scaled))
c(fitB1$deviance, -2*logLik(fitB1))
c(fitB1$null.deviance, -2*logLik(update(fitB1, formula = .~ 1)))

fitB2 <- update(fitB1, family = binomial(link = "probit"))
rbind(logit = fitB1$coefficients, probit = fitB2$coefficients, rescal.probit = fitB2$coefficients/0.5513)

# Model Quality
fitB3 <- update(fitB1, formula = .~ . + I(log(time)))
summary(rats$time[rats$status == 1])
summary(rats$time[rats$status == 0])
cbind(rats[1:10, ], model.matrix(fitB3)[1:10,])
summary(fitB3)$coef
c(2*(logLik(fitB3) - logLik(fitB1)), fitB1$deviance - fitB3$deviance)
1 - pchisq(20.22656, 1)

fitB4 <- update(fitB3, .~. + I(rx*log(time)) + I(log(time)^2))
summary(fitB4)$coef
fitB3$deviance - fitB4$deviance
anova(fitB4)

Devs <- c(fitB1$null.deviance, fitB3$deviance, update(fitB3, .~. + I(rx*log(time)))$dev, fitB4$deviance)
Devs
round(-diff(Devs), 3)
