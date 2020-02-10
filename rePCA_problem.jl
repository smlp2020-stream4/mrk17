using RData, DataFrames, DataFramesMeta, MixedModels, StatsBase
sleepstudy = deepcopy(dat[:sleepstudy])
colnames = ["Reaction", "Days", "Subject"]
names!(sleepstudy, Symbol.(colnames))
sleepstudy = @linq sleepstudy |>
             transform(Days2 = :Days .*2,
                       Days3 = :Days ./2)
fm1 = fit(LinearMixedModel, @formula(Reaction ~ 1 + Days + (1 + Days | Subject)), sleepstudy2)
fm1.rePCA

fm2 = fit(LinearMixedModel, @formula(Reaction ~ 1 + Days2 + (1 + Days2 | Subject)), sleepstudy2)
fm2.rePCA

fm3 = fit(LinearMixedModel, @formula(Reaction ~ 1 + Days3 + (1 + Days3 | Subject)), sleepstudy2)
fm3.rePCA





