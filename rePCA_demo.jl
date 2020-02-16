# Problem: rePCA() results not invariant with linear transformation of predictor
# Reinhold Kliegl, 2020-02-11

using  DataFrames, DataFramesMeta, MixedModels, StatsBase

# preinstalled datasets
MixedModels.datasets()

sleepstudy =  MixedModels.dataset(:sleepstudy)

colnames = ["Subj", "days", "reaction"]
rename!(sleepstudy, Symbol.(colnames))

sleepstudy = @linq sleepstudy |>
             transform(days2 = :days .*2,
                       days3 = :days ./2)

# rePCA issue 
fm1 = fit(LinearMixedModel, @formula(reaction ~ 1 + days + (1 + days | Subj)), sleepstudy)
fm1.rePCA
rePCA(fm1)
fm2 = fit(LinearMixedModel, @formula(reaction ~ 1 + days2 + (1 + days2 | Subj)), sleepstudy)]
fm2.rePCA

fm3 = fit(LinearMixedModel, @formula(reaction ~ 1 + days3 + (1 + days3 | Subj)), sleepstudy)
fm3.rePCA

# function rePCA(m::LinearMixedModel, corr::Bool=true; loadings::Bool=false)