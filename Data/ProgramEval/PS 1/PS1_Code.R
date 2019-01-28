# Set working directory
setwd("/Users/mariomoreno/Desktop/Grad School/ProgramEval/PS 1")

# Creating my sink file
sink(file='MexicoOutput')

#Importing files
install.packages("lmtest")
install.packages("stargazer")
install.packages("fastDummies")
library(tidyverse)
library(haven)
library(lmtest)
library(stargazer)
library(fastDummies)

# Reading in data
mex_data <- read_dta(file='SP_dataset.dta')

# 1) Inspecting the data

# Are there truly 100 clusters in 50 pairs
print('Number of clusters:')
print(length(unique(mex_data$cluster)))
print('Number of cluster pairs:')
print(length(unique(mex_data$clust_pair)))

# Does each pair have a treatment and control cluster
pair <- 1
while (pair < 51) {
  subs <- subset(mex_data, clust_pair == pair)
  print(unique(subs$treatment))
  pair <- pair + 1
}

# What is the modal level of schooling in the sample
## Removed the NAs because they were likely to be zero, so didn't factor into the sums
modal_list <- list(sum(mex_data$edu_info_051, na.rm=TRUE), sum(mex_data$edu_info_052, na.rm = TRUE), sum(mex_data$edu_info_053, na.rm = TRUE), sum(mex_data$edu_info_054, na.rm = TRUE), sum(mex_data$edu_info_055, na.rm = TRUE), sum(mex_data$edu_info_056, na.rm = TRUE))
print(modal_list)
# There is no mode.

# What is the modal marital status?
uniq <- unique(mex_data$marstat, na.rm=TRUE)
modal_mar <- uniq[which.max(tabulate(match(mex_data$marstat, uniq)))]
print(modal_mar)  
# What share of samples household were involved in Mexico's conditional cash program?
## Imputed missing with zero, divided sum of 1 over length
mex_data$opor_05[is.na(mex_data$opor_05)] <- 0
share = sum(mex_data$opor_05) / length(mex_data$opor_05)
print(share)
# What is mean non-health expenditure
## Found mean for 05 and 06, imputed missing variables as median
mex_data$allbut_05[is.na(mex_data$allbut_05)] = median(mex_data$allbut_05, na.rm=TRUE)
mex_data$allbut_06[is.na(mex_data$allbut_06)] = median(mex_data$allbut_06, na.rm=TRUE)
mean_05 = mean(mex_data$allbut_05)
mean_06 = mean(mex_data$allbut_06)


# 2) Which treatment parameters could be identified by these data?

# We are unable to identify average treatment effects from this data since
# its taken from a largely rural population. That means we will not be able
# to estimate the effects of the program on urban recipients. Furthermore, enrollment
# in the program is voluntary, which means that means that we'd run into a selection
# problem if we deployed an ATE approach given that people who select into the 
# program might have certain unobservable differences. This is why, from the
# data available, we can estimate average treatment effect on the treated and
# average treatment effect on the non-treated

# 3) Assess the adequacy of random assignment. You will need to think about how to use Stata's
# reg command to test for equal means among pre-intervention variables within cluster pairs
# How important is it to account for dependence within cluster? What is the bottom line
# Was random assignment properly executed

pair <- 1
while (pair < 51) {
  subz <- subset(mex_data, clust_pair == pair)
  print(t.test(subz$food_yr_05, subz$treatment, paired=TRUE))
  print(t.test(subz$allbut_05, subz$treatment, paired=TRUE))
  print(t.test(subz$oop_yr3_05, subz$treatment, paired=TRUE))
  pair <- pair + 1
}


# 4) Construct two variables relating health spending to household's total income
## fill in missing variables
mex_data$food_yr_05[is.na(mex_data$food_yr_05)] = median(mex_data$food_yr_05, na.rm=TRUE)
mex_data$food_yr_06[is.na(mex_data$food_yr_06)] = median(mex_data$food_yr_06, na.rm=TRUE)
mex_data$oop_yr3_05[is.na(mex_data$oop_yr3_05)] <- 0
mex_data$oop_yr3_06[is.na(mex_data$oop_yr3_06)] <- 0

## health budget share = health spending / total spending
mex_data$hbs_05 <- mex_data$oop_yr3_05 / (mex_data$food_yr_05 + mex_data$allbut_05 + mex_data$oop_yr3_05)
mex_data$hbs_06 <- mex_data$oop_yr3_06 / (mex_data$food_yr_06 + mex_data$allbut_06 + mex_data$oop_yr3_06)

## adjusted hbs = health spending / disposable income (total spending - food spending)
mex_data$adj_hbs_05 <- mex_data$oop_yr3_05 / (mex_data$allbut_05 + mex_data$oop_yr3_05)
mex_data$adj_hbs_06 <- mex_data$oop_yr3_06 / (mex_data$allbut_06 + mex_data$oop_yr3_06)

# 5) On average, what share of total expenditures does health spending account for?
mex_data$hbs_05[is.na(mex_data$hbs_05)] <- 0
mex_data$hbs_06[is.na(mex_data$hbs_06)] <- 0

health_spend_05 <- mean(mex_data$hbs_05)
health_spend_06 <- mean(mex_data$hbs_06)

## What are the 75 and 90 percentiles of the budget share distribution
quants <- quantile(mex_data$hbs_05, c(.75, 0.9))
qts <- quantile(mex_data$hbs_06, c(.75, .90))

## what share of disposible incomes does health spending account for
mex_data$adj_hbs_05[is.na(mex_data$adj_hbs_05)] <- 0
mex_data$adj_hbs_06[is.na(mex_data$adj_hbs_06)] <- 0

adj_health_spend_05 <- mean(mex_data$adj_hbs_05)
adj_health_spend_06 <- mean(mex_data$adj_hbs_06)

## What are the 75 and 90 percentiles of the adjusted budget share distribution
qtsadj <- quantile(mex_data$adj_hbs_05, c(.75, .9))
qtsadjo6 <- quantile(mex_data$adj_hbs_06, c(.75, .9))

# 6) Construct two measures of extreme health spending
## a dummy equal to one if household's health budget exceeds 20%
mex_data$ext_hbs_05[mex_data$hbs_05 >= 0.2] <- 1
mex_data$ext_hbs_05[mex_data$hbs_05 < 0.2] <- 0

mex_data$ext_hbs_06[mex_data$hbs_06 >= 0.2] <- 1
mex_data$ext_hbs_06[mex_data$hbs_06 < 0.2] <- 0

## a dummy equal to one if household's health budget exceeds 30% as portion of disposable
mex_data$ext_ahbs_05[mex_data$adj_hbs_05 >= 0.3] <- 1
mex_data$ext_ahbs_05[mex_data$adj_hbs_05 < 0.3] <- 0

mex_data$ext_ahbs_06[mex_data$adj_hbs_06 >= 0.3] <- 1
mex_data$ext_ahbs_06[mex_data$adj_hbs_06 < 0.3] <- 0

# 7) Estimate the ITT effects of the intervention on extreme health spending
## Regress the extreme health spending on the treatment dummy, calculate standard errors that account for dependence among households within clusters

regr05 <- lm(ext_hbs_05 ~ treatment, data = mex_data)
regr05adj <- lm(ext_ahbs_05 ~ treatment, data = mex_data)

regr06 <- lm(ext_hbs_06 ~ treatment, data = mex_data_dummies)
regr06adj <- lm(ext_ahbs_06 ~ treatment, data = mex_data_dummies)
  
## Add cluster-pair dummies, calculate standard errors that account for dependence among households within clusters 
mex_data_dummies <- cbind(mex_data, fastDummies::dummy_cols(mex_data$clust_pair))

regr051 <- lm(ext_hbs_05 ~ treatment + .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6 + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 + .data_15 + .data_16 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22 + .data_23 + .data_24 + .data_25 + .data_26 + .data_27 + .data_28 + .data_29 + .data_30 + .data_31 + .data_32 + .data_33 + .data_34 + .data_35 + .data_36 + .data_37 + .data_38 + .data_39 + .data_40 + .data_41 + .data_42 + .data_43 + .data_44 + .data_45 + .data_46 + .data_47 + .data_48 + .data_49 + .data_50, data = mex_data)
regr051adj <- lm(ext_ahbs_05 ~ treatment + .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6 + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 + .data_15 + .data_16 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22 + .data_23 + .data_24 + .data_25 + .data_26 + .data_27 + .data_28 + .data_29 + .data_30 + .data_31 + .data_32 + .data_33 + .data_34 + .data_35 + .data_36 + .data_37 + .data_38 + .data_39 + .data_40 + .data_41 + .data_42 + .data_43 + .data_44 + .data_45 + .data_46 + .data_47 + .data_48 + .data_49 + .data_50, data = mex_data)

regr061 <- lm(ext_hbs_06 ~ treatment + .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6 + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 + .data_15 + .data_16 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22 + .data_23 + .data_24 + .data_25 + .data_26 + .data_27 + .data_28 + .data_29 + .data_30 + .data_31 + .data_32 + .data_33 + .data_34 + .data_35 + .data_36 + .data_37 + .data_38 + .data_39 + .data_40 + .data_41 + .data_42 + .data_43 + .data_44 + .data_45 + .data_46 + .data_47 + .data_48 + .data_49 + .data_50, data = mex_data)
regr061adj <- lm(ext_ahbs_06 ~ treatment + .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6 + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 + .data_15 + .data_16 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22 + .data_23 + .data_24 + .data_25 + .data_26 + .data_27 + .data_28 + .data_29 + .data_30 + .data_31 + .data_32 + .data_33 + .data_34 + .data_35 + .data_36 + .data_37 + .data_38 + .data_39 + .data_40 + .data_41 + .data_42 + .data_43 + .data_44 + .data_45 + .data_46 + .data_47 + .data_48 + .data_49 + .data_50, data = mex_data)


stargazer(regr05, regr05adj, regr06, regr06adj, type="text", title = "Extreme Health on Treatment")
stargazer(regr051, regr051adj, regr061, regr061adj, type="text", title="Extremem Health on Treatment and Dummies")

## What happens to coefficients when you add the cluster pair dummies, to their standard errors, why? 

# 8) Now adjust for baseline characteristics
## Without dummies
regr_base05 <- lm(ext_hbs_05 ~ treatment, data = mex_data_dummies)
regr_base05adj <- lm(ext_ahbs_05 ~ treatment, data = mex_data_dummies)

regr_base06 <- lm(ext_hbs_06 ~ treatment, data = mex_data_dummies)
regr_base06adj <- lm(ext_ahbs_06 ~ treatment, data = mex_data_dummies)

## With dummies
regr05basedums <- lm(ext_hbs_05 ~ treatment + .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6 + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 + .data_15 + .data_16 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22 + .data_23 + .data_24 + .data_25 + .data_26 + .data_27 + .data_28 + .data_29 + .data_30 + .data_31 + .data_32 + .data_33 + .data_34 + .data_35 + .data_36 + .data_37 + .data_38 + .data_39 + .data_40 + .data_41 + .data_42 + .data_43 + .data_44 + .data_45 + .data_46 + .data_47 + .data_48 + .data_49 + .data_50, data = mex_data)
regr05baseadjdums <- lm(ext_ahbs_05 ~ treatment + .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6 + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 + .data_15 + .data_16 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22 + .data_23 + .data_24 + .data_25 + .data_26 + .data_27 + .data_28 + .data_29 + .data_30 + .data_31 + .data_32 + .data_33 + .data_34 + .data_35 + .data_36 + .data_37 + .data_38 + .data_39 + .data_40 + .data_41 + .data_42 + .data_43 + .data_44 + .data_45 + .data_46 + .data_47 + .data_48 + .data_49 + .data_50, data = mex_data)

regr06basedums <- lm(ext_hbs_06 ~ treatment + .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6 + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 + .data_15 + .data_16 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22 + .data_23 + .data_24 + .data_25 + .data_26 + .data_27 + .data_28 + .data_29 + .data_30 + .data_31 + .data_32 + .data_33 + .data_34 + .data_35 + .data_36 + .data_37 + .data_38 + .data_39 + .data_40 + .data_41 + .data_42 + .data_43 + .data_44 + .data_45 + .data_46 + .data_47 + .data_48 + .data_49 + .data_50, data = mex_data)
regr06baseadjdums <- lm(ext_ahbs_06 ~ treatment + .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6 + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 + .data_15 + .data_16 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22 + .data_23 + .data_24 + .data_25 + .data_26 + .data_27 + .data_28 + .data_29 + .data_30 + .data_31 + .data_32 + .data_33 + .data_34 + .data_35 + .data_36 + .data_37 + .data_38 + .data_39 + .data_40 + .data_41 + .data_42 + .data_43 + .data_44 + .data_45 + .data_46 + .data_47 + .data_48 + .data_49 + .data_50, data = mex_data)

stargazer(regr_base05, regr_base05adj, regr_base06, regr_base06adj, type='text', title='Extreme Health on Treatment with Baseline')
stargazer(regr05basedums, regr05baseadjdums, regr06basedums, regr06baseadjdums, type='text', title='Extreme Health on Treatment with Dummies and Baseline')

# 9 

# 10 