# Setup

pwd()
using CSV, DataFrames, MixedModels

dat = CSV.read("MRK17_Exp1_xtra.csv")
describe(dat)

m0form = @formula (-1000/rt) ~ 1+F*P*Q*lQ*lT + (1 | Subj) + (1 | Item);
oviLMM = fit(MixedModel, m0form, dat);
VarCorr(oviLMM)

show(oviLMM)

versioninfo()
