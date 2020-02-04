# Setup
cd(joinpath(homedir(),"Google Drive/ZiF_CG_WS2/MRK17_Exp1/"))
pwd()

using DataFramesMeta, DataFrames, InteractiveUtils, MixedModels, RData, RCall, CSV

# Works
dat = CSV.read("MRK17_Exp1_xtra.csv")
describe(dat)

# Complex LMM
## This is *not* the maximal LMM because we do not include interaction terms and
## associated correlation parameters in the RE structure; will add this LMM later.
@time cmplxLMM = fit(LinearMixedModel,
      @formula(rrt ~ 1 + F*P*Q*lQ*lT + (1+P+Q+lQ+lT | Item) + (1+F+P+Q+lQ+lT | Subj)), dat)

### Model summary
show(cmplxLMM)

# Zero-correlation parameter LMM
@time zcpLMM = fit(LinearMixedModel,
      @formula(rrt ~ 1 + F*P*Q*lQ*lT + zerocorr(1+P+Q+lQ+lT | Item) +
                                       zerocorr(1+F+P+Q+lQ+lT | Subj)), dat)
## Model summary
show(zcpLMM)

# Replication of final LMM in Masson & Kliegl (2013, Table 1) as well as
# reproduction of final lme4-based LMM in Masson, Rabe, & Kliegl (2017, Figure 2)
@time mrk17_LMM = fit(LinearMixedModel,
      @formula(rrt ~ 1 + F*P*Q*lQ*lT + zerocorr(1+P | Item) + (1+Q | Subj) +
                                       zerocorr(0+lT | Subj)), dat)

show(mrk17_LMM)

# Appendix
versioninfo()