cd(joinpath(homedir(),"Documents/ZiF_CG_WS2/MRK17_Exp1/"))
pwd()
using DataFramesMeta, DataFrames, InteractiveUtils, LinearAlgebra, MixedModels, RData, RCall, CSV

# Read Urdaten
dat0 = CSV.read("MRK17_Exp1.csv")

# Does not work
#dat = load("MRK17_Exp1_xtra.RData") |>
#      select(:rrt, :F, :T, :Q, :lQ, :lT, :Subj, :Item)

# Works
dat = CSV.read("MRK17_Exp1_xtra.csv")

@time m1 = fit(LinearMixedModel,
      @formula(rrt ~ 1 + F*T*Q*lQ*lT + (1+T+Q+lQ+lT | Item) + (1+F+T+Q+lQ+lT | Subj)), dat)

show(m1)
m1.optsum

m1.λ[1]  # for shown target (Item)
m1.λ[2]  # for subject (Subj)

var1 = abs2.(svdvals(m1.λ[1]));
show(var1)

var2 = abs2.(svdvals(m1.λ[2]));
show(var2)

show(cumsum(var1) ./ sum(var1))
show(cumsum(var2) ./ sum(var2))

versioninfo()
