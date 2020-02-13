# SEMANTIC PRIMING with high/low frequency related/unrelated targets presented in clear or dim font
# Masson & Kliegl (2013, JEPLMC; Exp 1: random presentation of all conditions)
# 15 June 2013,  R. Kliegl

# PARTS of Script
# (1) LMMs only w/ varying intercepts for subj and items (quick preliminary checks)
# (2) Determination of "optimal" LMM (i.e., LMM m5) wrt var components and corr parameters
# (3) m5a: LMM m5 reparameterization for F x P nested within W by L 
# (4) m5b: LMM m5 reparameterization for Q x F nested within L 
# (5) Figures on the basis of LMM m5

# (6) Adding Trial as covariate to LMM m5 -> LMM x6, 
#     incl. various reparatmetrizations and alternative models
# (7) Figure for Trial x Lag-Target on the basis of LMM x6

library(memisc)

library(lme4)
library(plyr)
library(ggplot2)

rm(list=ls())
source("functions/mtable-ext.R")  # requires package memisc
source("functions/remef.v0.6.7.R")


load("Exp1.rda")
# Dependent variables:  
# rt  is response time in ms
# rrt = -1000/rt  ; indicated by boxcox() (see Exp1.setup.R)


# PART 1: LMMs only w/ varying intercepts for subj and items (quick preliminary checks)


# Basic factorial LMM
print(m0 <- lmer(rrt ~ 1 + W*L*Q*F*P + (1 | id)  + (1 | st), data=d), cor=FALSE)

# ... same pattern when lag-1 error trials are excluded
print(n0 <- lmer(rrt ~ 1 + W*L*Q*F*P + (1 | id)  + (1 | st), data=d.lsc), cor=FALSE)

# Is the F x P interaction oppositely significant within levels of Lag_Target by LagQuality? YES! see m1
d$WLFP <- factor(paste(substr(d$w, 1, 1), substr(d$l, 1, 1), substr(d$f, 1, 1), substr(d$p, 1, 1), sep="_") )

# ... re-order alphabetically sorted levels to correspond to factorial design
d$WLFP <- factor(d$WLFP, levels=c(levels(d$WLFP)[9:16], levels(d$WLFP)[1:8]) )

# Setup matrix for f x p as nested within levels of w x l
cmat   <- matrix(c(            -1/8, -1/8, -1/8, -1/8, -1/8, -1/8, -1/8, -1/8, +1/8, +1/8, +1/8, +1/8, +1/8, +1/8, +1/8, +1/8,
                               -1/8, -1/8, -1/8, -1/8, +1/8, +1/8, +1/8, +1/8, -1/8, -1/8, -1/8, -1/8, +1/8, +1/8, +1/8, +1/8,
                               +1/8, +1/8, +1/8, +1/8, -1/8, -1/8, -1/8, -1/8, -1/8, -1/8, -1/8, -1/8, +1/8, +1/8, +1/8, +1/8,
                               -1/2, -1/2, +1/2, +1/2,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
                               -1/2, +1/2, -1/2, +1/2,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
                               +1/2, -1/2, -1/2, +1/2,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
                                  0,    0,    0,    0, -1/2, -1/2, +1/2, +1/2,    0,    0,    0,    0,    0,    0,    0,    0,
                                  0,    0,    0,    0, -1/2, +1/2, -1/2, +1/2,    0,    0,    0,    0,    0,    0,    0,    0,
                                  0,    0,    0,    0, +1/2, -1/2, -1/2, +1/2,    0,    0,    0,    0,    0,    0,    0,    0,
                                  0,    0,    0,    0,    0,    0,    0,    0, -1/2, -1/2, +1/2, +1/2,    0,    0,    0,    0,
                                  0,    0,    0,    0,    0,    0,    0,    0, -1/2, +1/2, -1/2, +1/2,    0,    0,    0,    0,
                                  0,    0,    0,    0,    0,    0,    0,    0, +1/2, -1/2, -1/2, +1/2,    0,    0,    0,    0,
                                  0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0, -1/2, -1/2, +1/2, +1/2,
                                  0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0, -1/2, +1/2, -1/2, +1/2,
                                  0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0, +1/2, -1/2, -1/2, +1/2 ), 16, 15)

cmat.i <- fractions(t(ginv(cmat)))
rownames(cmat.i) <- levels(d$WLFP)
colnames(cmat.i) <- c(".lW", ".lQ", ".lW_lQ", ".f1", ".p1", ".fp1", ".f2", ".p2", ".fp2", ".f3", ".p3", ".fp3", ".f4", ".p4", ".fp4")
(contrasts(d$WLFP) <- cmat.i)

print(m1 <- lmer(rrt ~ 1 + WLFP*Q + (1 | id) + (1 | st), data=d), cor=FALSE)

# ... ... check for equality of logLik of m0 and m1
anova(m0, m1)


# PART 2: Determination of "optimal" LMM wrt significant variance components and correlation parameters


# Test variance components (note: F = between-items)
print(m2 <- lmer(rrt ~ 1 + W*L*Q*F*P + (1 | id) + (1 | st) +
	              (0+W | id) + (0+L | id) + (0+Q | id) + (0+F | id) + (0+P | id) +
	              (0+W | st) + (0+L | st) + (0+Q | st)              + (0+P | st), data=d), cor=FALSE)

# ... very small variance components (see m2 output) do not contribute to goodness of fit
print(m3 <- lmer(rrt ~ 1 + W*L*Q*F*P + (1 | id) + (0+W | id) + (0+Q | id) + (1 | st) + (0+P | st), data=d), cor=FALSE)

anova(m0, m2, m3)

# ... each of the remaining variance components is significant
print(m3.Wid <- lmer(rrt ~ 1 + W*L*Q*F*P + (1 | id) + (0+Q | id) + (1 | st) + (0+P | st), data=d), cor=FALSE)
anova(m3.Wid, m3)

print(m3.Qid <- lmer(rrt ~ 1 + W*L*Q*F*P + (1 | id) + (0+W | id) + (1 | st) + (0+P | st), data=d), cor=FALSE)
anova(m3.Qid, m3)

print(m3.Pst <- lmer(rrt ~ 1 + W*L*Q*F*P + (1 | id) + (0+W | id) + (0+Q | id)  + (1 | st), data=d), cor=FALSE)
anova(m3.Pst, m3)

# ... test of correlation parameters is significant,
print(m4 <- lmer(rrt ~ 1 + W*L*Q*F*P + (1+W+Q | id) + (1+P | st), data=d), cor=FALSE)
anova(m3, m4)

# ... but only one of them is significant: Mean speed and effect of stimulus quality: -0.41
print(m5 <- lmer(rrt ~ 1 + W*L*Q*F*P + (1+Q | id) + (0+W | id) + (1 | st) + (0+P | st), data=d), cor=FALSE)

anova(m3, m5, m4)
attr(VarCorr(m5)[4]$id, "correlation")  # -0.41
# LMM m5 is the declared the "optimal" LMM for this data (reported in Table 1, p. 904)

# Reproducing Table 1 with additional constraint of no-lag-1 error trials and with untransformed RTs as DV
print(m5.lsc <- lmer(rrt ~ 1 + W*L*Q*F*P + (1+Q | id) + (0+W | id) + (1 | st) + (0+P | st), data=d.lsc), cor=FALSE)
print(m5.rt <- lmer(rt ~ 1 + W*L*Q*F*P + (1+Q | id) + (0+W | id) + (1 | st) + (0+P | st), data=d), cor=FALSE)
print(m5.rt.lsc <- lmer(rt ~ 1 + W*L*Q*F*P + (1+Q | id) + (0+W | id) + (1 | st) + (0+P | st), data=d.lsc), cor=FALSE)

mtable(m5, m5.lsc, m5.rt, m5.rt.lsc, coef.style="horizontal")

# Different correlation parameters are significant for LMMs w/ rrt and rt!a
print(m3.rt <- lmer(rt ~ 1 + W*L*Q*F*P + (1 | id) + (0+W | id) + (0+Q | id) + (1 | st) + (0+P | st), data=d), cor=FALSE)
print(m4.rt <- lmer(rt ~ 1 + W*L*Q*F*P + (1+W+Q | id) + (1+P | st), data=d), cor=FALSE)
print(m6.rt <- lmer(rt ~ 1 + W*L*Q*F*P + (1| id) + (0+W+Q| id) + (1 | st) + (0+P | st), data=d), cor=FALSE)
anova(m3.rt, m4.rt, m6.rt)
attr(VarCorr(m6.rt)$id, "correlation")  # -0.54
# Effect of lag_target and effect of stimulus quality correlate -0.54

# Compare residuals for LMM m5 and LMM m6.rt
qqmath(resid(m5))
qqmath(resid(m6.rt))


# PART 3: Reparameterize LMM m5 with f x p nested in w x l


# Reuse  d$WLFP, generated for m1 (see above)
print(m5a <- lmer(rrt ~ 1 + WLFP*Q + (1+Q | id) + (0+W | id) + (1 | st) + (0+P | st), data=d), cor=FALSE)

# ... check for equality of logLik
anova(m5, m5a)


# PART 4: Reparameterized LMM m5 with q x f nested in l 


d$LQF <- factor(paste(substr(d$l, 1, 1), substr(d$q, 1, 1), substr(d$f, 1, 1), sep="_") )

# Setup matrix for q x f nested within levels of l
cmat2   <- matrix(c(-1/4, -1/4, -1/4, -1/4, +1/4, +1/4, +1/4, +1/4,
                    -1/2, -1/2, +1/2, +1/2,    0,    0,    0,    0,
                    -1/2, +1/2, -1/2, +1/2,    0,    0,    0,    0,
                    +1/2, -1/2, -1/2, +1/2,    0,    0,    0,    0,
                       0,    0,    0,    0, -1/2, -1/2, +1/2, +1/2,
                       0,    0,    0,    0, -1/2, +1/2, -1/2, +1/2,
                       0,    0,    0,    0, +1/2, -1/2, -1/2, +1/2), 8, 7)

cmat2.i <- fractions(t(ginv(cmat2)))
rownames(cmat2.i) <- levels(d$LQF)
colnames(cmat2.i) <- c(".lQ", ".q1", ".f1", ".qf1", ".q2", ".f2", ".qf2")
(contrasts(d$LQF) <- cmat2.i)

print(m5b <- lmer(rrt ~ 1 + LQF*P*W + (1+Q | id) + (0+W | id) + (1 | st) + (0+P | st), data=d), cor=FALSE)

# ... ... check for equality of logLik
anova(m5, m5b)


# PART 5: FIGURES (note: figure style was slightly modified for publication)

# Copy and relevel factors for plots
d <- transform(d, 
		   Lag_Target = factor(w, levels=c("W", "NW"), labels= c("N-1: Word", "N-1: Nonword")),
		   Lag_Target2 = factor(w, levels=c("NW", "W"), labels= c("N-1: Nonword", "N-1: Word")),
		   Frequency = factor(f, levels=c("LF", "HF"), labels=c("Low", "High")),
		   Priming = factor(p, levels=c("Rel", "Unr"), labels=c("Related", "Unelated")),
		   Priming2 = factor(p, levels=c("Unr", "Rel"), labels=c("Unrelated", "Related")),
		   Quality = factor(q, levels=c("Degraded", "Clear")),
		   Lag_Quality = factor(l, levels=c("Degraded", "Clear"), labels= c("N-1: Degraded", "N-1: Clear")),
		   Lag_Quality2 = factor(l, levels=c("Clear", "Degraded"), labels= c("N-1: Clear", "N-1: Degraded")))

# Figure 2. Frequency x Priming x Quality (p. 905), based on regular ANOVA of RTs for subjects
# ... F1-ANOVA
d_subj <- ddply(d, .(id, Frequency, Priming2, Quality), summarise, RT=mean(rt) )
summary(aov(RT ~ Frequency*Priming2*Quality + Error(id/(Frequency*Priming2*Quality)), data=d_subj))

# ... Table of means and within-subject SEs 
#     (Loftus & Masson, 1994; Cousineau, 2005; Morey, 2008)
# ... ... grand mean of RT
GM <- mean(d_subj$RT)

# ... ... remove between-subject variance; C = Cousineau (2005)
d_subj <- ddply(d_subj, .(id), transform, RT_C = RT - mean(RT) + GM)

Table.fig2 <- ddply(d_subj, .(Frequency, Priming2, Quality), summarise, 
			  N=length(RT), M=mean(RT), SD=sd(RT), 
			  SE_C=sd(RT_C)/sqrt(N), CI_C=SE_C*qt(.975, N-1) )

# ... ... adjust SEs for number of measures; M = Morey (2008), mf=Moray factor
nl <- nlevels(d_subj$Frequency)*nlevels(d_subj$Priming2)*nlevels(d_subj$Quality) 
mf <- sqrt( nl/(nl-1) )  

Table.fig2$SE_M <- Table.fig2$SE_C*mf
Table.fig2$CI_M <- Table.fig2$SE_M*qt(.975, Table.fig2$N-1)

# ... generate figure 
(Plot.fpq <- ggplot(data=Table.fig2, aes(x=Frequency, y=M, group=Quality:Priming2)) +
 	xlab("Word Frequency") + ylab("Response Time [ms]") + 
 	geom_errorbar( aes(ymax=M+2*SE_M, ymin=M-2*SE_M, width=0.03) ) +
 	geom_line(aes(linetype=Quality:Priming2)) +
 	geom_point(aes(shape=Quality:Priming2), size=3, fill = "white") + 			 
 	scale_linetype_manual("Priming", values=c(2, 2, 1, 1)) +
 	scale_shape_manual("Priming", values=c(21, 19, 21, 19) ) +
 	theme_bw() + theme(legend.title=element_blank()) + 
 	theme(legend.justification=c(1, 1), legend.position=c(1, 1)))  

# Alternative for Morey-based within-subject SEs and CIs (Winston Chang)
# source("functions/normDataWithin.R")
# source("functions/summarySE.R")
# source("functions/summarySEwithin.R")
# summarySEwithin(data=d_subj, idvar="id", measurevar="RT", withinvars=c("f", "p", "q"))

# Figures based on LMM m5 

# Figure 3. Quality x Lag_Quality x Lag Word/Nonword (p. 906)
d$wlq <- remef(m5, keep=TRUE, grouping=TRUE, fix = c(1, "W:L:Q"), ran = NULL) 

# ... Ms and SEs
Table.fig3 <- ddply(d, .(Lag_Target, Lag_Quality, q), summarise, N=length(wlq), M=mean(wlq), SE=sd(wlq)/sqrt(N)) 

# ... generate figure 
(Plot.wlq <- ggplot(data=Table.fig3, aes(x=q, y=M, group=Lag_Quality) ) +
	        xlab("Stimulus Quality") +
              facet_grid(. ~ Lag_Target) +
 	        geom_errorbar( aes(ymax=M+2*SE, ymin=M-2*SE, width=0.03) ) +
 	        geom_line() + geom_point(aes(shape=Lag_Quality), size=3, fill = "white") + 			 
 	        scale_y_continuous("Response Time  [-1/RT]", breaks=seq(from=-1.80, to=-1.50, by=.05)) +
 	        scale_shape_manual("Previous target", values=c(21, 19, 21, 19) ) +
 	        coord_cartesian(ylim=c(-1.80, -1.50) ) + theme_bw() )	


# Figure 4.  Frequency x Priming x Lag_Quality x Lag Word/Nonword (p.906)
d$wlfp <- remef(m5, keep=TRUE, grouping=TRUE, fix = c(1, "W:L:F:P"), ran = NULL) 

# ... Ms and SEs
Table.fig4 <- ddply(d, .(Lag_Target, Lag_Quality2, Frequency, Priming2), summarise, N= length(wlfp), M=mean(wlfp), SE=sd(wlfp)/sqrt(N)) 

# ... generate figure 
(Plot.wlfp <- ggplot(data=Table.fig4, aes(x=Frequency, y=M, group=Priming2)) +
                   facet_grid(Lag_Quality2 ~ Lag_Target)  + 
 	             geom_errorbar( aes(ymax=M+2*SE, ymin=M-2*SE, width=0.03) ) + 
 	             geom_line() + geom_point(aes(shape=Priming2), size=3, fill = "white") + 
 	             scale_y_continuous("Response Time  [-1/s]", breaks=seq(from=-1.80, to=-1.50, by=.05)) +  
 	             scale_shape_manual("Priming", values=c(21, 19) ) +
 	             coord_cartesian(ylim=c(-1.80, -1.50) ) + theme_bw() +
 	             theme(legend.justification=c(1, 0), legend.position=c(1, 0)))


# Figure 5. Frequency x Quality x Lag_Quality (p.906)
d$lqf <- remef(m5, keep=TRUE, grouping=TRUE, fix = c(1, "L:Q:F"), ran = NULL) 

# ... Ms and SEs
Table.fig5 <- ddply(d, .(Lag_Quality2, Quality, Frequency), summarise, N=length(lqf), M=mean(lqf), SE=sd(lqf)/sqrt(N)) 

# ... generate figure 
(Plot.lqf <- ggplot(data=Table.fig5, aes(x=Frequency, y=M, group=Quality)) +
                   facet_grid(. ~ Lag_Quality2) +
                   geom_errorbar( aes(ymax=M+2*SE, ymin=M-2*SE, width=0.03) ) +
 	             geom_line() + geom_point(aes(shape=Quality), size=3, fill = "white") + 
 	             scale_y_continuous("Response Time  [-1/s]", breaks=seq(from=-1.80, to=-1.50, by=.05)) +  
 	             scale_shape_manual("Quality", values=c(21, 19) ) +
 	             coord_cartesian(ylim=c(-1.80, -1.50) ) + theme_bw() +
 	             theme(legend.justification=c(1, 1), legend.position=c(1, 1)))


# -----------------------------------------------

# PART 6: LMM m5 plus trial as covariate = LMM x6 


# LMM with trial as covariate
print(x6  <- lmer(rrt ~ 1 + W*L*Q*F*P + poly(trial.c, 2) + W:trial.c +
				(1+Q | id) + (0+W | id) + (1 | st) + (0+P | st), data=d, REML=TRUE), cor=FALSE)

# Reparameterization of LMM x6 with f x p nested within levels of w x l
print(x6a  <- lmer(rrt ~ 1 + WLFP*Q + poly(trial.c, 2) + W:trial.c +
(1+Q | id) + (0+W | id) + (1 | st) + (0+P | st), data=d, REML=TRUE), cor=FALSE)

# ... ... check for equality of logLik
anova(x6a, x6)

# Reparameterization of LMM x6 with q x f nested within levels of l
print(x6b <- lmer(rrt ~ 1 + LQF*P*W + poly(trial.c, 2) + W:trial.c +
		     	(1+Q | id) + (0+W | id) + (1 | st) + (0+P | st), data=d), cor=FALSE)

# ... ... check for equality of logLik
anova(x6, x6b)

# Reparametrization of trial effects within panels of last Target, 
# ... that is separate estimates for trial for lag-word and lag-nonword targets,
# but main effect of quadratic trend for trial
d$lW_trial <- ifelse(d$W==-0.5, d$trial.c, 0)
d$lNW_trial <- ifelse(d$W==+0.5, d$trial.c, 0)

print(x6c <- lmer(rrt ~ 1 +  W*L*Q*F*P + lW_trial + lNW_trial + poly(trial.c, 2)[ ,2] +
		     	(1+Q | id) + (0+W | id) + (1 | st) + (0+P | st), data=d), cor=FALSE)

anova(x6, x6c) 


# Various alternatives (as above for LMM m5)
print(x6.lsc  <- lmer(rrt ~ 1 + W*L*Q*F*P + poly(trial.c, 2) + W:trial.c +
			    	(1+Q | id) + (0+W | id) + (1 | st) + (0+P | st), data=d.lsc, REML=TRUE), cor=FALSE)

print(x6.rt  <- lmer(rt ~ 1 + W*L*Q*F*P + poly(trial.c, 2) + W:trial.c +
			   	(1+Q | id) + (0+W | id) + (1 | st) + (0+P | st), data=d, REML=TRUE), cor=FALSE)

print(x6.rt.lsc  <- lmer(rt ~ 1 + W*L*Q*F*P + poly(trial.c, 2) + W:trial.c +
				 (1+Q | id) + (0+W | id) + (1 | st) + (0+P | st), data=d.lsc, REML=TRUE), cor=FALSE)

mtable(x6, x6.lsc, x6.rt, x6.rt.lsc, coef.style="horizontal")

# Compare residuals for LMM m5 and LMM x6
qqmath(resid(m5))
qqmath(resid(x6))

resdf.m5 <- data.frame(model="m5", x=fitted(m5), y=resid(m5))
resdf.x6 <- data.frame(model="x6", x=fitted(x6), y=resid(x6))
resdf <- rbind(resdf.m5, resdf.x6)
qplot(data=resdf, x=x, y=y, geom="point", shape=I("."), facets=. ~ model,	
	xlab="Fitted values", ylab="Standardized residuals") + 
	geom_hline(yintercept=0) + theme_bw() + 
	geom_density2d(size=1) 
#     geom_hex() + geom_density2d(size=0.2, col="white")



# PART 7: Figures based on LMM x6


# Figure 6. Trial x Lag Target (Word vs. Nonword)
d$wt  <- remef(x6, keep=TRUE, 
		   fix = c(1, "W", "poly(trial.c, 2)1",  "poly(trial.c, 2)1", "W:trial.c"), ran = NULL)

# ... partial effects in transformed rt (i.e., -1000/rt)
(Plot.wt <- ggplot(d, aes(x=trial, y=wt) ) + xlab("Trial") + xlab("Trial") +
 	geom_smooth( aes(group = Lag_Target2, colour=Lag_Target2), method="lm", formula=y~poly(x, 1), size=1) +
 	scale_colour_manual("N-1 Target", values=c("red", "blue")) +
 	scale_y_continuous("Response Time  [-1/s]", breaks=seq(from=-1.80, to=-1.50, by=.05)) +  
 	coord_cartesian(ylim=c(-1.80, -1.50) ) ) + theme_bw()

# ... observed untransformed RT
(ggplot(d, aes(x=trial, y=rt) ) + xlab("Trial") + 
 	geom_smooth( aes(group = Lag_Target2, colour=Lag_Target2), method="lm", formula=y~poly(x, 1), size=1) +
 	scale_colour_manual("N-1 Target", values=c("red", "blue")) +
 	scale_y_continuous("Response Time [ms]", breaks=seq(from=0, to=1000, by=20)) +  
 	coord_cartesian(ylim=c(600, 700) ) + theme_bw() )

# Added 20 June 2013, 
# Test Mike's proposal to look whether trial x type of last target (W vs. NW) is also
# significant for unrelated vs. related last type of target trials (all of which were word trials)
table(d$lpr, d$w)
table(d$ltt, d$w)

# Version 1: Select on trials where last trial was also a word target
d2 <- droplevels(subset(d, w == "W"))  # pt = prev
table(d2$lpr, d2$w)

d2$LPR <- ifelse(d2$lpr == "Rel",  -1/2, +1/2)

print(x7  <- lmer(rrt ~ 1 + LPR*L*Q*F*P + poly(trial.c, 2) + LPR:trial.c +
				(1+Q | id) + (0+LPR | id) + (1 | st) + (0+P | st), data=d2, REML=TRUE), cor=FALSE)
print(x7a  <- lmer(rrt ~ 1 + LPR+L+Q+F+P + poly(trial.c, 2)  + L:Q + LPR:P +
				(1+Q | id) + (0+LPR | id) + (1 | st) + (0+P | st), data=d2, REML=TRUE), cor=FALSE)

d2$xt  <- remef(x7, keep=TRUE, grouping=TRUE,
		   fix = c(1, "LPR:P", "poly(trial.c, 2)1",  "poly(trial.c, 2)1", "LPR:trial.c"), ran = NULL)

# ... partial effects in transformed rt (i.e., -1000/rt)
(Plot.xt <- ggplot(d2, aes(x=trial, y=xt) ) + xlab("Trial") + 
 	geom_smooth( aes(group = lpr, colour=lpr), method="lm", formula=y~poly(x, 2), size=1) +
 	scale_colour_manual("N-1 Target", values=c("red", "blue")) +
 	scale_y_continuous("Response Time  [-1/s]", breaks=seq(from=-1.80, to=-1.50, by=.05)) +  
 	coord_cartesian(ylim=c(-1.70, -1.60) ) ) + theme_bw()

d2$p_lpr  <- remef(x7, keep=TRUE, grouping=TRUE, fix = c(1, "LPR:P", ran = NULL))
Table.lpr_p <- ddply(d2, .(p, lpr), summarise, N=length(p_lpr), M=mean(p_lpr), SD=sd(p_lpr), SE=SD/sqrt(N))


# ... generate figure 
(Plot.lpr_p <- ggplot(data=Table.lpr_p, aes(x=p, y=M, group=lpr)) +
 	geom_errorbar( aes(ymax=M+2*SE, ymin=M-2*SE, width=0.03) ) +
 	geom_line() + geom_point(aes(shape=lpr), size=3, fill = "white") + 
 	scale_y_continuous("Response Time  [-1/s]", breaks=seq(from=-1.80, to=-1.50, by=.05)) +  
 	scale_shape_manual("LastRelation", values=c(21, 19) ) +
 	coord_cartesian(ylim=c(-1.80, -1.50) ) + theme_bw() +
 	theme(legend.justification=c(1, 1), legend.position=c(1, 1)))

# Version 2: Respecify factor w as lt (last target type) with 3 levels/2 contrasts: LP = unrW -relW; LW = unrNW - unrW
d$lt <- ifelse(d$w == "NW", "UnrNW",  ifelse(d$lpr == "Rel", "RelW", "UnrW"))
d$lt <- factor(d$lt, levels=c("RelW", "UnrW", "UnrNW"))
table(d$lt, d$w)
	
contrasts(d$lt) <- MASS::contr.sdif(3)
d$LP <- ifelse(d$lt == "RelW", -2/3, 1/3)
d$LW <- ifelse(d$lt == "UnrNW", 2/3, -1/3)

print(x8  <- lmer(rrt ~ 1 + (LP + LW)*L*Q*F*P + poly(trial.c, 2) + (LP + LW):trial.c +
					(1+Q | id) + (0+LP | id) + (0+LW | id) + (0 + trial.c | id) + (1 | st) + (0+P | st), data=d, REML=TRUE), cor=FALSE)
	
print(x8a  <- lmer(rrt ~ 1 + LP*P + LW*L*Q + L*Q*F + poly(trial.c, 2) + LP:trial.c + LW:trial.c +
				(1+Q | id) + (0+LP | id) + (0+LW | id) + (0 + trial.c | id) + (1 | st) + (0+P | st), data=d, REML=TRUE), cor=FALSE)

print(x8b  <- lmer(rrt ~ 1 + LP*P + LW*L*Q + L*Q*F + poly(trial.c, 2) + LP:trial.c + LW:trial.c +
				 	(1+Q | id) + (0+LW | id) + (0 + trial.c | id) + (1 | st) + (0+P | st), data=d, REML=TRUE), cor=FALSE)
		
anova(x8b,  x8)	

#
d$xt  <- remef(x8b, keep=TRUE, grouping=TRUE,
		   fix = c(1, "poly(trial.c, 2)1",  "poly(trial.c, 2)1", "LP:trial.c", "LW:trial.c"), ran = NULL)

# ... partial effects in transformed rt (i.e., -1000/rt), but where is the priming effect?
(Plot.xt <- ggplot(d, aes(x=trial, y=xt) ) + xlab("Trial") + 
 	geom_smooth( aes(group = lt, colour=lt), method="lm", formula=y~poly(x, 2), size=1) +
 	scale_colour_manual("N-1 Target", values=c("red", "blue", "black")) +
 	scale_y_continuous("Response Time  [-1/s]", breaks=seq(from=-1.80, to=-1.50, by=.05)) +  
 	coord_cartesian(ylim=c(-1.70, -1.60) ) ) + theme_bw()

d$pxt  <- remef(x8b, keep=TRUE, grouping=TRUE,
			   fix = c(1, "poly(trial.c, 2)1",  "poly(trial.c, 2)1", "LP:trial.c", "LW:trial.c"), ran = NULL)

# ... partial effects in transformed rt (i.e., -1000/rt), but where is the priming effect?
(Plot.pxt <- ggplot(d, aes(x=trial, y=pxt) ) + xlab("Trial") + facet_grid(. ~ p) +
	 	geom_smooth( aes(group = lt, colour=lt), method="lm", formula=y~poly(x, 2), size=1) +
	 	scale_colour_manual("N-1 Target", values=c("red", "blue", "black")) +
	 	scale_y_continuous("Response Time  [-1/s]", breaks=seq(from=-1.80, to=-1.50, by=.05)) +  
	 	coord_cartesian(ylim=c(-1.75, -1.50) ) ) + theme_bw()

Table <- ddply(d, .(p), summarise, N=length(pxt), M=mean(pxt), SD=sd(pxt), SE=SD/sqrt(N))
	
# ... observed effect in transformed rt (i.e., -1000/rt)
(Plot.rrt <- ggplot(d, aes(x=trial, y=rrt) ) + xlab("Trial") + 
	 	geom_smooth( aes(group = lt, colour=lt), method="lm", formula=y~poly(x, 2), size=1) +
	 	scale_colour_manual("N-1 Target", values=c("red", "blue", "black")) +
	 	scale_y_continuous("Response Time  [-1/s]", breaks=seq(from=-1.80, to=-1.50, by=.05)) +  
	 	coord_cartesian(ylim=c(-1.70, -1.60) ) ) + theme_bw()
	