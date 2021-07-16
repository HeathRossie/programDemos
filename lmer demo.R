library(faraway)
library(MASS)
library(car)
library(pbkrtest)
data(jsp)
mod <- lmer(english ~ raven * gender * math * social * gender + (1|school:class:id) + (1|year), data = jsp)
anova(mod)
summary(mod)
df.residual(mod)

confint.merMod(mod, method='Wald')
Anova(mod)

mod <- lmer(english ~ raven  + (1|school), data = jsp)
mod2 <- lmer(english ~ raven + gender + (1|school), data = jsp)
PBmodcomp.merMod(mod2, mod)
KRmodcomp(mod2,
  mod)

mod2 <- lmer(english ~ raven + (1|school:class:id) + (1|year), data = jsp)
confint.merMod(mod2, method='Wald')
mod2


