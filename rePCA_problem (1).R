# Problem: rePCA() results not invariant with linear transformation of predictor
# Reinhold Kliegl, 2020-02-10

library(lme4)

fm1 <- lmer(Reaction ~ 1 + Days + (1 + Days | Subject), sleepstudy, REML=FALSE)
VarCorr(fm1)
print(summary(rePCA(fm1)))

Days2 <- sleepstudy$Days*2
fm2 <- lmer(Reaction ~ Days2 + (1 + Days2 | Subject), sleepstudy, REML=FALSE)
VarCorr(fm2)         # This is to be expected
summary(rePCA(fm2))  # This not so much

Days3 <- sleepstudy$Days/2
fm3 <- lmer(Reaction ~ Days3 + (1 + Days3 | Subject), sleepstudy, REML=FALSE)
VarCorr(fm3)         # This is to be expected
summary(rePCA(fm3))  # This not so much
