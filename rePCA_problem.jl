# Problem: rePCA() results not invariant with linear transformation of predictor
# Reinhold Kliegl, 2020-02-10

using  DataFrames, DataFramesMeta, MixedModels, StatsBase, Feather

#sleepstudy = deepcopy(dat[:sleepstudy])
#colnames = ["reaction", "days", "subj"]
#names!(sleepstudy, Symbol.(colnames))

sleepstudy =  Feather.read(joinpath(MixedModels.TestData, "sleepstudy.feather"))

sleepstudy = @linq sleepstudy |>
             transform(days2 = :days .*2,
                       days3 = :days ./2,
                       Days = categorical(:days))

# rePCA issue 
fm1 = fit(LinearMixedModel, @formula(reaction ~ 1 + days + (1 + days | subj)), sleepstudy)
fm1.rePCA

fm2 = fit(LinearMixedModel, @formula(reaction ~ 1 + days2 + (1 + days2 | subj)), sleepstudy)
fm2.rePCA

fm3 = fit(LinearMixedModel, @formula(reaction ~ 1 + days3 + (1 + days3 | subj)), sleepstudy)
fm3.rePCA

# fulldummy
# ... baseline
fm4 = fit(LinearMixedModel, @formula(reaction ~ 0 + Days + (1 + Days | subj)), sleepstudy)

# ... problem
fm5 = fit(LinearMixedModel, @formula(reaction ~ 0 + Days + fulldummy(0 + Days | subj)), sleepstudy)

# ... problem
fm6 = fit(LinearMixedModel, @formula(reaction ~ 0 + Days + fulldummy(1 + Days | subj)), sleepstudy)




