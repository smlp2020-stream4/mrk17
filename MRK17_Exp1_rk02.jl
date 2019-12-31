# Setup
cd(joinpath(homedir(),"Documents/ZiF_CG_WS2/MRK17_Exp1/"))
pwd()

# Reading data

### We read the data preprocessed with R and saved as RDS file
using DataFrames, MixedModels, RCall

R"dat_r = readRDS('MRK17_Exp1.rds')"
dat = rcopy(R"dat_r")
describe(dat)

# Model fitting

## Coding of two-level factors
### The contrasts for the two-level factors use the Helmert coding, which is ±1.

const HC = HelmertCoding();
contrasts = Dict(n => HC for (n,v) in eachcol(dat, true) if length(levels(v)) == 2)

## Complex LMM
### This is *not* the maximal LMM because we do not include interaction terms and
### associated correlation parameters in the RE structure; will add this LMM later.
m1form = @formula (-1000/rt) ~ 1+F*P*Q*lQ*lT +
                              (1+F+P+Q+lQ+lT | Subj) +
                              (1+P+Q+lQ+lT | Item)
@time cmplxLMM = fit(MixedModel, m1form, dat, contrasts=contrasts)

### VCs and CPs
### ... Item-related estimated covariance matrix
cmplxLMM.λ[1]
### ... and Subj-related VCs and CPs
cmplxLMM.λ[2]

### Cumulative proportions of the variance
cmplxLMM.rePCA

### Model summary
show(cmplxLMM)

## Zero-correlation parameter LMM
m2form = @formula (-1000/rt) ~ 1 + F*P*Q*lQ*lT +
                               zerocorr(1+F+P+Q+lQ+lT | Subj) +
                               zerocorr((1+P+Q+lQ+lT | Item)

@time zcpLMM = fit(LinearMixedModel, m2form, dat, contrasts=contrasts)

### VCs and CPs
### ... Item-related estimated covariance matrix
zcpLMM.λ[1]
### ... and Subj-related VCs and CPs
zcpLMM.λ[2]

### Cumulative proportions of the variance
zcpLMM.rePCA

### Model summary
show(zcpLMM)

## Parsimonious LMM
### Replication of final LMM in Masson & Kliegl (2013, Table 1) as well as
### reproduction of final lme4-based LMM in Masson, Rabe, & Kliegl (2017, Figure 2)
m3form = @formula (-1000/rt) ~ 1 + F*P*Q*lQ*lT +
                               (1+Q | Subj) + zerocorr(0+lT | Subj) +
                               zerocorr(1+P | Item)
@time mrk17_LMM = fit(LinearMixedModel, m3form, dat, contrasts=contrasts)

### VCs and CPs
### ... Subj-related estimated covariance matrix
mrk17_LMM.λ[1]
### ... and Item-related VCs and CPs
mrk17_LMM.λ[2]

### Cumulative proportions of the variance
mrk17_LMM.rePCA


### Model summary
show(mrk17_LMM)

# Alternative parsimonious LMM
m4form = @formula (-1000/rt) ~ 1 + F*P*Q*lQ*lT +
                               (1+Q | Subj) + (0+lT | Subj) +
                               zerocorr(1+P | Item)
@time mrk17_LMM_b = fit(LinearMixedModel, m4form, dat, contrasts=contrasts)

### Model summary
show(mrk17_LMM_b)

# Doug's version
m5form = @formula (-1000/rt) ~ 1 + F*P*Q*lQ*lT + (1+Q | Subj) + (0+lT | Subj) + zerocorr(1+P | Item)
m5 = fit(MixedModel, m5form, dat, contrasts=contrasts)

### Model summary
show(m5)
# Appendix
versioninfo()
