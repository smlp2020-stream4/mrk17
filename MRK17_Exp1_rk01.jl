# Setup
cd(joinpath(homedir(),"Documents/ZiF_CG_WS2/MRK17_Exp1/"))
pwd()

using DataFramesMeta, DataFrames, InteractiveUtils, LinearAlgebra, MixedModels, RData, RCall, CSV, ShiftedArrays

# Does not work
#dat = load("MRK17_Exp1_xtra.RData") |>
#      select(:rrt, :F, :T, :Q, :lQ, :lT, :Subj, :Item)

# Read Urdaten
dat0 = CSV.read("MRK17_Exp1.csv")
describe(dat0)

# These transformations illustrate some Julia functionality, ...
# ... but they do not yet match what is needed.
dat1 = @linq dat0 |>
    transform(lagQlty = lag(:Qlty), lagTrgt = lag(:Freq)) |>
    where(:Freq .≠ "NW", :Score .== "C", 30 .≤ :rt .≤ 3000) |>
    select(:Subj, :Item, :trial, :Freq, :Prime, :Qlty, :lagQlty, :lagTrgt, :rt)
describe(dat1)

# Works
dat = CSV.read("MRK17_Exp1_xtra.csv")
describe(dat)

# Complex LMM
## This is *not* the maximal LMM because we do not include interaction terms and
## associated correlation parameters in the RE structure; will add this LMM later.
@time cmplxLMM = fit(LinearMixedModel,
      @formula(rrt ~ 1 + F*P*Q*lQ*lT + (1+P+Q+lQ+lT | Item) + (1+F+P+Q+lQ+lT | Subj)), dat)

## Item-related VCs and CPs
cmplxLMM.λ[1]                        # estimated covariance matrix
var1 = abs2.(svdvals(cmplxLMM.λ[1])) # squares of singular values on the scale ...
show(var1)                          # ... of variances of principal components
show(cumsum(var1) ./ sum(var1))     # cumulative proportion of the variance

## Subject-related VCs and CPs
cmplxLMM.λ[2]
var2 = abs2.(svdvals(cmplxLMM.λ[2]))
show(var2)
show(cumsum(var2) ./ sum(var2))       # overparameterized

## Model summary
show(cmplxLMM)

# Zero-correlation parameter LMM
@time zcpLMM = fit(LinearMixedModel,
      @formula(rrt ~ 1 + F*P*Q*lQ*lT + zerocorr((1+P+Q+lQ+lT | Item)) +
                                       zerocorr((1+F+P+Q+lQ+lT | Subj))), dat)

## Item-related VCs
zcpLMM.λ[1]
var1 = abs2.(svdvals(zcpLMM.λ[1]))
show(var1)
show(cumsum(var1) ./ sum(var1))       # overparameterized

## Subject-related VCs
zcpLMM.λ[2]
var2 = abs2.(svdvals(zcpLMM.λ[2]));
show(var2)
show(cumsum(var2) ./ sum(var2))       # overparameterized

# Replication of final LMM in Masson & Kliegl (2013, Table 1) as well as
# reproduction of final lme4-based LMM in Masson, Rabe, & Kliegl (2017, Figure 2)
@time mrk17_LMM = fit(LinearMixedModel,
      @formula(rrt ~ 1 + F*P*Q*lQ*lT + zerocorr((1+P | Item)) + (1+Q | Subj) +
                                       zerocorr((0+lT | Subj))), dat)

## Item-related VCs
mrk17_LMM.λ[1]
var1 = abs2.(svdvals(mrk17_LMM.λ[1]))
show(var1)
show(cumsum(var1) ./ sum(var1))       # overparameterized

## Subject-related VCs
mrk17_LMM.λ[2]
var2 = abs2.(svdvals(mrk17_LMM.λ[2]));
show(var2)
show(cumsum(var2) ./ sum(var2))       # overparameterized

show(mrk17_LMM)

# Appendix
versioninfo()
