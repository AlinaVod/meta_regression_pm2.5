#########################################################
##
## Nonlinear concentraton-response function: Health impact Calculation applying penalized spline model
##
## Date February 27, 2019
## by Alina Vodonos Zilberg (avodonos@hsph.harvard.edu)
#########################################################

library(readr)
library(dplyr)
library(foreign)
library(mgcv)
library(tidyr)

#### Data frame (df) must contatin the following variables;
# Example data frame.
#Let’s say we want to calculate the excess deaths addtibutble to reducing the PM2.5 levels from ‘pm2.5_high’ to ‘pm2.5_low’, 
#first we need to compute the average slope between those two concentration points.


df <- data.frame(pm2.5_high = 15,
                 pm2.5_low = 10,
                 deaths = 3)

# Step 1- Calculating the mean beta using the numeric integral from the meta-regrssion penalized spline predictions

df["delta_pm"]<-df$pm2.5_high-df$pm2.5_low
df["delta_pm_by20"]<-I(df$delta_pm/20)

### A loop to add delta_pm_by20 to starting from pm2.5_low
varnames = paste0('INC',seq(1,20,1))
for (i in 1:20){
  df$tempvar = df$pm2.5_low + df$delta_pm_by20*i  
  names(df)[names(df) == 'tempvar'] = varnames[i]
}

df$seqvar = seq(1:nrow(df)) # set a sequence of observations
df.long <- df %>% gather(INC, INC_Value, INC1:INC20)
df.long.order<-with(df.long, df.long[order(seqvar),]) # make sure it is ordered
df.long.order$pm25<-df.long.order$INC_Value
#Coeffitions from the meta-regression model. Change if nedded. There are a different slopes for each outcome;
# Outcome=1 -> all cause mortality
# Outcome=2 -> cardiovascular mortality
# Outcome=3 -> lung cancer mortality
# Outcome=4 -> cardiopulmonary mortality
# Outcome=9 -> respiratory mortality 

df.long.order$Outcome<-1 
df.long.order$elderly<-0 # elderly=1, age above 65
df.long.order$female<-0

### Loading the meta-regression model
load("/metaregression.RData")
pred.df.long.order<-predict(mod1$gam, type = "response", df.long.order, se=T)
df.long.order <-cbind(df.long.order, pred.df.long.order ) # merge predictions with the df

### The numeric integral
df.long.order$pi_delta_pm<-df.long.order$fit*df.long.order$delta_pm_by20
df.long.order$pi_delta_pm_se<-df.long.order$se.fit*df.long.order$delta_pm_by20

df.agg<-df.long.order %>%
  group_by(seqvar) %>%
  summarise(Sum_pi_delta_pm=sum(pi_delta_pm,na.rm=T),
            Sum_pi_delta_pm_se=sum(pi_delta_pm_se, na.rm = T),
            pm2.5_low = mean(pm2.5_low, na.rm=TRUE), 
            pm2.5_high = mean(pm2.5_high, na.rm=TRUE),
            delta_pm=mean(delta_pm, na.rm=T),
            deaths=mean(deaths, na.rm=T))

rm(df.long, df.long.order, pred.df.long.order)
### Step 2- Calculating the ExcessDeaths and the AF

df.agg["mean_beta"]<-df.agg$Sum_pi_delta_pm/df.agg$delta_pm
df.agg["mean_beta_se"]<-df.agg$Sum_pi_delta_pm_se/df.agg$delta_pm
df.agg["mean_beta_low"]<-(df.agg$mean_beta-(1.96*df.agg$mean_beta_se))
df.agg["mean_beta_high"]<-(df.agg$mean_beta+(1.96*df.agg$mean_beta_se))

df.agg["ExcessDeaths"]<-((exp(df.agg$mean_beta*df.agg$delta_pm)-1)*df.agg$deaths)/((exp(df.agg$mean_beta*df.agg$delta_pm)))
df.agg["ExcessDeaths_low"]<-((exp(df.agg$mean_beta_low*df.agg$delta_pm)-1)*df.agg$deaths)/((exp(df.agg$mean_beta_low*df.agg$delta_pm)))
df.agg["ExcessDeaths_high"]<-((exp(df.agg$mean_beta_high*df.agg$delta_pm)-1)*df.agg$deaths)/((exp(df.agg$mean_beta_high*df.agg$delta_pm)))

df.agg["AF"]<-((exp(df.agg$mean_beta*df.agg$delta_pm)-1))/((exp(df.agg$mean_beta*df.agg$delta_pm)))*100
df.agg["AF_low"]<-((exp(df.agg$mean_beta_low*df.agg$delta_pm)-1))/((exp(df.agg$mean_beta_low*df.agg$delta_pm)))*100
df.agg["AF_high"]<-((exp(df.agg$mean_beta_high*df.agg$delta_pm)-1))/((exp(df.agg$mean_beta_high*df.agg$delta_pm)))*100







