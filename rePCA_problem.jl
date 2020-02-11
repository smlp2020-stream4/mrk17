# Problem: rePCA() results not invariant with linear transformation of predictor
# Reinhold Kliegl, 2020-02-11

using  DataFrames, DataFramesMeta, MixedModels, StatsBase, Feather

sleepstudy =  Feather.read(joinpath(MixedModels.TestData, "sleepstudy.feather"))

colnames = ["Subj", "days", "reaction"]
rename!(sleepstudy, Symbol.(colnames))

sleepstudy = @linq sleepstudy |>
             transform(days2 = :days .*2,
                       days3 = :days ./2)

# rePCA issue 
fm1 = fit(LinearMixedModel, @formula(reaction ~ 1 + days + (1 + days | Subj)), sleepstudy)
fm1.rePCA

fm2 = fit(LinearMixedModel, @formula(reaction ~ 1 + days2 + (1 + days2 | Subj)), sleepstudy)
fm2.rePCA

fm3 = fit(LinearMixedModel, @formula(reaction ~ 1 + days3 + (1 + days3 | Subj)), sleepstudy)
fm3.rePCA

