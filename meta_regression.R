######--------------------------------------------------------    Meta-regression   --------------------------------------
library(readr)
library(mgcv)

meta_regression <- read_csv("codes/meta_analysis_full_RA.csv")
#Spline model
mod1<-gamm(B~s(pm25,bs="cr")+factor(Outcome)+elderly+female, weights=varFixed(~I(SE^2)), random=list( cohort=~1, Study2=~1),family=gaussian(), 
           data=meta_regression, control = lmeControl(sigma = 1, apVar = FALSE))
summary(mod1$lme)
summary(mod1$gam)
plot(mod1$gam,scale=0)



save(mod1, file="original_data/meta_regression.Rdata")
