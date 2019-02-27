#########################################################
##
## Nonlinear concentraton-response function: Health impact Calculation applying parametric function
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

# Those are the parameters from the model
B0 <- 0.0059 #intercept slope
B1 <- 0.0705 # 1/pm2.5 slope
B0var <- 1.408353e-05  #varcov[1,1]
B1var <- 0.002551382  #varcov[2,2]
B0B1cov <- -0.0001884276  #varcov[1,2]

# Area under y (from x = a to x = )  = F(b) -F(a)
#F(pm2.5_high) -F(pm2.5_low) = log(pm2.5_high) – log(pm2.5_low).

df$pm2.5_high_log <- log(df$pm2.5_high)
df$pm2.5_low_log <- log(df$pm2.5_low)
df$delta_pm <- (df$pm2.5_high - df$pm2.5_low)

df["mean_beta"] <- B0 + (B1 * ((df$pm2.5_high_log - df$pm2.5_low_log) / df$delta_pm ))
df["mean_beta_var"] <- B0var + B1var * ((df$pm2.5_high_log - df$pm2.5_low_log) / df$delta_pm ) ^ 2 + 2 * (B0B1cov * (df$pm2.5_high_log - df$pm2.5_low_log) / df$delta_pm)
df["mean_beta_var_se"] <- sqrt(df$mean_beta_var)
df["mean_beta_low"] <- (df$mean_beta - (1.96 * df$mean_beta_var_se))
df["mean_beta_high"] <- (df$mean_beta + (1.96 * df$mean_beta_var_se))

df["ExcessDeaths"] <- ((exp(df$mean_beta * df$delta_pm) - 1) * df$deaths) / ((exp(df$mean_beta * df$delta_pm)))
df["ExcessDeaths_low"] <- ((exp(df$mean_beta_low * df$delta_pm) - 1) * df$deaths) / ((exp(df$mean_beta_low * df$delta_pm)))
df["ExcessDeaths_high"] <- ((exp(df$mean_beta_high * df$delta_pm) - 1) * df$deaths) / ((exp(df$mean_beta_high * df$delta_pm)))

df["AF"]<-((exp(df$mean_beta*df$delta_pm)-1))/((exp(df$mean_beta*df$delta_pm)))*100
df["AF_low"]<-((exp(df$mean_beta_low*df$delta_pm)-1))/((exp(df$mean_beta_low*df$delta_pm)))*100
df["AF_high"]<-((exp(df$mean_beta_high*df$delta_pm)-1))/((exp(df$mean_beta_high*df$delta_pm)))*100

df



