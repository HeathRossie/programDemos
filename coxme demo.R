library(coxme)
library(lme4)
?coxme

fit0 <- coxme(Surv(time, status) ~ 1 + (ph.ecog|inst), data=lung)
fit <- coxme(Surv(time, status) ~ ph.ecog + (ph.ecog|inst), data=lung)

summary(fit)

fixef(fit)
ranef(fit)

ranef(fit)[[1]]
unlist(ranef(fit))

plot(tapply(lung$ph.karno, lung$inst, mean, na.rm = TRUE), unlist(ranef(fit)))
