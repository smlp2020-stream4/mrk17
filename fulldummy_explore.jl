# Use fulldummy() for correlation (of GM and day) means of categorical timeseries
# Reinhold Kliegl, 2020-02-11

using  DataFrames, DataFramesMeta, MixedModels, StatsBase, Feather

sleepstudy =  Feather.read(joinpath(MixedModels.TestData, "sleepstudy.feather"))

sleepstudy = @linq sleepstudy |>
             transform(Days = categorical(:days))

# Exploring fulldummy
# ... default with CPs, no intercept
fm1 = fit(LinearMixedModel, @formula(reaction ~ 0 + Days + (0 + Days | subj)), sleepstudy)

# ... with intercept
fm2 = fit(LinearMixedModel, @formula(reaction ~ 0 + Days + (1 + fulldummy(Days) | subj)), sleepstudy)

# ... zerocorr
fm3 = fit(LinearMixedModel, @formula(reaction ~ 0 + Days + zerocorr(1 + fulldummy(Days) | subj)), sleepstudy)

