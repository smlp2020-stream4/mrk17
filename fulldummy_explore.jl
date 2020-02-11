# Use fulldummy() for correlation (of GM and day) means of categorical timeseries
# Reinhold Kliegl, 2020-02-11

using  DataFrames, DataFramesMeta, MixedModels, StatsBase, Feather

sleepstudy =  Feather.read(joinpath(MixedModels.TestData, "sleepstudy.feather"))

colnames = ["Subj", "days", "reaction"]
rename!(sleepstudy, Symbol.(colnames))

sleepstudy = @linq sleepstudy |>
             transform(Days = categorical(:days))

# Exploring fulldummy
# ... default with CPs, no intercept
fm1 = fit(LinearMixedModel, @formula(reaction ~ 0 + Days + (0 + Days | Subj)), sleepstudy)

# ... with intercept
fm2 = fit(LinearMixedModel, @formula(reaction ~ 0 + Days + (1 + fulldummy(Days) | Subj)), sleepstudy)

# ... zerocorr
fm3 = fit(LinearMixedModel, @formula(reaction ~ 0 + Days + zerocorr(1 + fulldummy(Days) | Subj)), sleepstudy)

