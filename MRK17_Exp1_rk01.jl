cd(joinpath(homedir(),"Documents/ZiF_CG_WS2/MRK17_Exp1/"))
pwd()
using DataFramesMeta, DataFrames, InteractiveUtils, LinearAlgebra, MixedModels, RData, RCall, CSV, ShiftedArrays

# Read Urdaten
dat0 = CSV.read("MRK17_Exp1.csv")
describe(dat0)

# These transformations illustrate some Julia functionality, but they do not match what is needed.
dat1 = @linq dat0 |>
    transform(lagQlty = lag(:Qlty), lagTrgt = lag(:Freq)) |>
    where(:Freq .≠ "NW", :Score .== "C", 30 .≤ :rt .≤ 3000) |>
    select(:Subj, :Item, :trial, :Qlty, :Freq, :TPR, :lagQlty, :lagTrgt, :rt)
describe(dat1)

# Does not work
#dat = load("MRK17_Exp1_xtra.RData") |>
#      select(:rrt, :F, :T, :Q, :lQ, :lT, :Subj, :Item)

# Works
dat = CSV.read("MRK17_Exp1_xtra.csv")
describe(dat)

# full LMM
@time fullLMM = fit(LinearMixedModel,
      @formula(rrt ~ 1 + F*T*Q*lQ*lT + (1+T+Q+lQ+lT | Item) + (1+F+T+Q+lQ+lT | Subj)), dat)

## Item-related VCs and CPs
fullLMM.λ[1]
var1 = abs2.(svdvals(fullLMM.λ[1]))
show(var1)
show(cumsum(var1) ./ sum(var1))

## Subject-related VCs and CPs
fullLMM.λ[2]
var2 = abs2.(svdvals(fullLMM.λ[2]));
show(var2)
show(cumsum(var2) ./ sum(var2))

## All model parameters
show(fullLMM)

# zero-correlation parameter LMM
@time zcpLMM = fit(LinearMixedModel,
      @formula(rrt ~ 1 + F*T*Q*lQ*lT + zerocorr((1+T+Q+lQ+lT | Item)) + zerocorr((1+F+T+Q+lQ+lT | Subj))), dat)

## Item-related VCs
zcpLMM.λ[1]
var1 = abs2.(svdvals(zcpLMM.λ[1]))
show(var1)
show(cumsum(var1) ./ sum(var1))

## Subject-related VCs
zcpLMM.λ[2]
var2 = abs2.(svdvals(zcpLMM.λ[2]));
show(var2)
show(cumsum(var2) ./ sum(var2))

## All model parameters
show(zcpLMM)

# reduced LMM
@time redLMM = fit(LinearMixedModel,
      @formula(rrt ~ 1 + F*T*Q*lQ*lT + zerocorr((1 | Item)) + zerocorr((1+Q | Subj))), dat)

## Item-related VCs
redLMM.λ[1]
var1 = abs2.(svdvals(redLMM.λ[1]))
show(var1)
show(cumsum(var1) ./ sum(var1))

## Subject-related VCs
redLMM.λ[2]
var2 = abs2.(svdvals(redLMM.λ[2]));
show(var2)
show(cumsum(var2) ./ sum(var2))

show(redLMM)

# reduced-CP-extended LMM
@time extLMM = fit(LinearMixedModel,
      @formula(rrt ~ 1 + F*T*Q*lQ*lT + zerocorr((1 | Item)) + (1+Q | Subj)), dat)

## Item-related VCs
extLMM.λ[1]
var1 = abs2.(svdvals(extLMM.λ[1]))
show(var1)
show(cumsum(var1) ./ sum(var1))

## Subject-related VCs
extLMM.λ[2]
var2 = abs2.(svdvals(extLMM.λ[2]));
show(var2)
show(cumsum(var2) ./ sum(var2))

show(extLMM)

# Appendix
versioninfo()
