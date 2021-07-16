mass <- c(25, 14, 68, 79, 64, 139, 49, 119, 111)
pop <- factor(c(1,1,1,2,2,2,3,3,3))
length <- c(1,14,22,2,9,20,2,13,22)

lm <- lm(mass ~ pop-1 + length)
library(lme4)
lmm <- lmer(mass ~ length + (1|pop))

plot(length, mass, col=as.integer(pop), pch = 16, cex = 2)
abline(lm$coef[1], lm$coef[4], col = "black", lwd = 2)
abline(lm$coef[2], lm$coef[4], col = "red", lwd = 2)
abline(lm$coef[3], lm$coef[4], col = "green", lwd = 2)
abline(fixef(lmm)[1] + ranef(lmm)[[1]][1,], fixef(lmm)[2], col = "black", lwd = 2, lty = 2)
abline(fixef(lmm)[1] + ranef(lmm)[[1]][2,], fixef(lmm)[2], col = "red", lwd = 2, lty = 2)
abline(fixef(lmm)[1] + ranef(lmm)[[1]][3,], fixef(lmm)[2], col = "green", lwd = 2, lty = 2)

