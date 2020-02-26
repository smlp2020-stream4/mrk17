# In the R-based lme4 package, rePCA() results are not invariant with linear transformation of predictor
# if based on (relative) covariance matrix.
# In the Julia-based MixedModels package, by default they are based on the correlation, not the covariance matrix. 
# The covariance-based version is available with MixedModels.PCA(model, corr=false). 

using DataFrames, DataFramesMeta, MixedModels
using StatsBase, StatsModels, BenchmarkTools

# preinstalled datasets
MixedModels.datasets()

sleepstudy =  MixedModels.dataset(:sleepstudy)

colnames = ["Subj", "days", "reaction"]
rename!(sleepstudy, Symbol.(colnames))

sleepstudy = @linq sleepstudy |>
             transform(days2 = :days .*2,
                       days3 = :days ./2)

# Fit some models: fm2 and fm3 use a linear transformation of days
fm1 = fit(LinearMixedModel, @formula(reaction ~ 1 + days  + (1 + days | Subj)), sleepstudy)
fm2 = fit(LinearMixedModel, @formula(reaction ~ 1 + days2 + (1 + days2 | Subj)), sleepstudy)
fm3 = fit(LinearMixedModel, @formula(reaction ~ 1 + days3 + (1 + days3 | Subj)), sleepstudy)

# rePCA propoerty use corr=true -- normalized cumulative proportion of variance is the same
fm1.rePCA
fm2.rePCA
fm3.rePCA

# equivalent info from MixedModels.PCA(... corr=true, ...) ...
MixedModels.PCA(fm1, corr=true)
MixedModels.PCA(fm2, corr=true)
MixedModels.PCA(fm3, corr=true)

# ... but not for corr=false. 
MixedModels.PCA(fm1, corr=false)
MixedModels.PCA(fm2, corr=false)
MixedModels.PCA(fm3, corr=false)
# These normalized cumulative proportions of variance are reported with lme4::rePCA()

# MixedModel.PCA() default options availabe with show() are:
# Base.show(pca::PCA;
#           ndigitsmat=2, ndigitsvec=2, ndigitscum=4,
#           covcor=true, loadings=true, variances=false, stddevs=false)
fm1_pca_f = MixedModels.PCA(fm1, corr=false);
show(fm1_pca_f.Subj, covcor=false, loadings=false, variances=false, stddevs=false)  
# Note: Must add grouping identifier!

fm2_pca_f = MixedModels.PCA(fm2, corr=false);
show(fm2_pca_f.Subj, covcor=false, loadings=false, variances=false, stddevs=false)  

