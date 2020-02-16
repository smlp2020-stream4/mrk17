# rePCA() results not invariant with linear transformation of predictor
# if based on (relative) covariance matrix. This is the new default now.
# So we are looking at normalized cumulative variance proportions.
# Reinhold Kliegl, 2020-02-16

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
fm0 = fit(LinearMixedModel, @formula(reaction ~ 1 + days + (1  | Subj)), sleepstudy)
fm0.rePCA

fm1 = fit(LinearMixedModel, @formula(reaction ~ 1 + days + (1 + days | Subj)), sleepstudy)
fm1.rePCA
MixedModels.PCA.(fm1.reterms, corr=true)
MixedModels.PCA.(fm1.reterms, corr=false)

fm2 = fit(LinearMixedModel, @formula(reaction ~ 1 + days2 + (1 + days2 | Subj)), sleepstudy)
fm2.rePCA
MixedModels.PCA.(fm2.reterms, corr=true)
MixedModels.PCA.(fm2.reterms, corr=false)

fm3 = fit(LinearMixedModel, @formula(reaction ~ 1 + days3 + (1 + days3 | Subj)), sleepstudy)
fm3.rePCA
MixedModels.PCA.(fm3.reterms, corr=true)
MixedModels.PCA.(fm3.reterms, corr=false)

# Another newby: LRT test
MixedModels.likelihoodratiotest(fm0, fm1)
