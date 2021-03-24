### QUESTION 1
# use the datasets 'pinelands_bees.csv' and 'land_cover.csv' to answer the question:
# Does natural land cover in the surrounding landscape predict the number of individual bees collected at a site?
# follow the analysis workflow that we learned in class, for which there are prompts below
# make sure to complete all 6 prompts
# use comment lines to briefly explain your reasoning at each step (ie, why you are doing what you are doing)
# you will turn in your code (ie, this R file) to your github repo at the end of class

## brief metadata
# the datasets you will use are 'pinelands_bees.csv'  and 'land_cover.csv'
# these are data I collected on wild bees of 117 ish species (ish because the taxonomy of some specimens is not fully sorted out) 
# data were collected at 27 study sites in the New Jersey pinelands
# all sites were in forest habitat, but sites differed in how much of the surrounding landscape was forested
# bees were collected on four days at each site between April and September 2003, with collection events organized into rounds, such that each site was sampled once in round 1 before round 2 was begun, etc
# 'pinelands_bees.csv'contains data on the bees collected, where each row is one individual bee
# 'land_cover.csv' column 'Nat1600' is the percentage of the natural land cover surrounding the site within a 1600m radius (roughly an 800 ha area), where the natural habitat is predominantly forest, with a small amount of other natural habitats such as open wetlands

library(MuMIn)
library(dplyr)
library(ggplot2)
library(stats)
library(broom)

##  1 Get and format the data
# you will need to create a new dataframe that has the variables you need: site, land cover, number of bees collected
# you may want to use group_by, summarize, n(), left_join

setwd("C:/Users/piedm/Dropbox/R/GitandR/christensen_tyler")
bees<-read_csv("pinelands_bees.csv")
land<-read_csv("land_cover.csv")
glimpse(bees)
glimpse(land)
bees2<-select(bees, genus_species, site_name)

bees_per_site<-as.data.frame(count(bees2, site_name))
glimpse(bees_per_site)

bees_per_site$site_name<-as.factor(bees_per_site$site_name)
bees_per_site$n<-as.numeric(bees_per_site$n)
glimpse(bees_per_site)

bees3<-merge(bees_per_site, land, by = "site_name")

## 2 Data picture
# plot the data and figure out what type of model might be best

ggplot(data = bees3, aes(x = Nat1600, y = n))+
    geom_point()

hist(bees3$n)

###The data does not appear to be normally distributed, and the residuals are not similar across values of x.
###Does not appear very normal...

bees4 <- mutate(bees3, log_n = log(bees3$n))

hist(bees4$log_n)

###tried log-transforming the data to get a little closer to normality

## 3 Test model assumptions
# you can test the assumptions of more than one type of model here if you aren't sure
# explain your reasoning for the model you choose

beemod<-lm(n ~ Nat1600, data = bees3)
autoplot(beemod)
summary(beemod)

beemod2<-lm(log_n ~ Nat1600, data = bees4)
autoplot(beemod2)
summary(beemod2)

shapiro.test(bees3$n)
shapiro.test(bees4$log_n)

###after log-transforming the data it followed an approximately normal distribution.  A shapiro-Wilk test showed the untransformed data did not follow a normal distribution.  
###The same test on the log-transformed n, on the other hand, failed to indicate a non-normal distribution.

###After the transformation, the residuals vs. fitted plot showed a more even distribution of residuals across x.
###The qq-plot showed similar improvement; the outliers, particularly at the high end of the distribution (large Nat1600 values) were more tamed
###The residuals vs. leverage plot indicates that the outliers don't have a huge amount of influence on the model

## 4 Report and interpret results
# make sure you interpret the following things: coefficients, p value, explanatory value of model
# state your conclusions using numbers and units
# discuss the magnitude and biological significance of your result, not just its significance
# how confident are you in this analysis and why

### The intercept coefficient (4.19) indicated that the log of the number of individual bees is highest when the amount of natural land cover is 0, which may not make a lot of sense in reality (0 natural land cover may mean few nesting or foraging opportunities)
### The slope coefficient indicates that with every increase in one unit along the Nat1600 axis, the log number of bees decreases by about 0.008.
### This relationship only just achieves statistical significance (p = 0.047), indicating a low probability that this model could have resulted from a dataset in which there was no real relationship between the number of bees and the amount of natural land cover
###The shallow slope indicates this relationship is not particularly strong

## 5 Plot model results back onto the data picture
# geom_smooth is the easy way to do this, but you can alternatively do it manually using the model output (coefficients) if you want


beemod3<-lm(n ~ Nat1600, data = bees4)

beemod3aug = augment(beemod3, data=bees4, type.predict="response", se_fit=T)
beemod3aug

beemod3aug <- mutate(beemod3aug, ci_lwr = .fitted - (1.96 * .se.fit), ci_upr = .fitted + (1.96 * .se.fit))
beemod3aug

ggplot(data = beemod3aug, aes(x = Nat1600, y = n)) + 
    geom_point() + 
    geom_line(aes(x = Nat1600, y = .fitted)) +
    geom_ribbon(data = beemod3aug, aes(ymin = ci_lwr, ymax = ci_upr), alpha = 0.1) 


## 6  If you were to add a random effect to this model, what would it be, what would adding it accomplish?
# please answer this question in comment lines
# you do not need to format any data here or code or run anything
# explain your reasoning for using this random effect
# you will need to go back to the original data file and metadata to answer this question
# extra credit: write out the model code for your model now including the random effect

###adding a random effect of "site" into this model wouldn't make sense, since in this model there is only one observation per site (the number of bees seen there)
###However, we could account for season or month by creating a nested dataset that includes both site and month, and setting date as the random effect.  Using date as a random
###effect would control for any circumstantial influence it may have over the model (for example, maybe remote sites were sampled later in the season)



### QUESTION 2
# The file "modSel.csv" contains simulated dataset of observations of a focal species at a series of sites.
# For each site, you have observed abundance, and measurements of environmental variables you hypothesize
# to affect the distribution of this species.
# Specifically, you hypothesize the species is more abundant in warmer, wetter regions,
# and that it prefers core over edge habitat.
# You have measured each of these in a couple ways, as mean annual and summer temperature,
# cumulative annual and summer precipitation, and distance to nearest edge and total edge within 500 m.
# Your goal here is to find the best model you can, given your hypotheses,
# to describe the distribution of this species.
# In doing so, you will also assess the more relevant measure of each environmental condition,
# and weigh their relative importance (or at least predictive power).
# For simplicity, do not consider interactions between variables.
# Please give your models interpretable names.

setwd("C:/Users/piedm/Dropbox/R/GitandR/christensen_tyler")
modSel<- read.csv("modSel.csv")

names(modSel)

# Step 1. Find the best error structure/error distribution for these data.
# State your conclusion in comment lines
# (Hints: you want to assess model-error distributions, not the data distribution; these are count data.)

mod1<-glm(observedAbundance ~ meanAnnualTemp, data = modSel)
hist(mod1$residuals)

global1<-glm(observedAbundance ~ meanAnnualTemp + meanSummerTemp + annualPrecipitation + summerPrecipitation + distance2edge + totalEdge, family = "poisson", data = modSel)
hist(global2$residuals)

###the residuals are not normally distributed; because these are count data whose mean value is close to 0, they cannot be normally distributed.  I'm going to use
###a poisson distribution for the error structure.

# Step 2: Having determined the best error structure, determine the more effective method of measuring each variable.
# For each variable, compare methods as a pair of single-variable models (e.g., summer temp vs annual temp).
# State your conclusion in comment lines

ab_by_annual_temp<-glm(observedAbundance ~ meanAnnualTemp, family = "poisson", data = modSel)
ab_by_summer_temp<-glm(observedAbundance ~ meanSummerTemp, family = "poisson", data = modSel)
model.sel(ab_by_annual_temp, ab_by_summer_temp)

###keep summer temperature

ab_by_annual_precip<-glm(observedAbundance ~ annualPrecipitation, family = "poisson", data = modSel)
ab_by_summer_precip<-glm(observedAbundance ~ summerPrecipitation, family = "poisson", data = modSel)
model.sel(ab_by_annual_precip, ab_by_summer_precip)

###keep summer precip

ab_by_edge_dist<-glm(observedAbundance ~ distance2edge, family = "poisson", data = modSel)
ab_by_edge_amt<-glm(observedAbundance ~ totalEdge, family = "poisson", data = modSel)
model.sel(ab_by_edge_dist, ab_by_edge_amt)

###keep total edge

###the retained variables are summer temperature, summer precipitation, and total edge

# Step 3: Having determined which method of measurement for each variable is best,
# determine the most effective combination of predictors;
# run a set of competing models and create a table comparing these models to each other and to a null.
# state your conclusion in comment lines

global_mod<-glm(observedAbundance ~ meanSummerTemp + summerPrecipitation + totalEdge, family = "poisson", data = modSel)
ab_by_temp_and_precip<-glm(observedAbundance ~ meanSummerTemp + summerPrecipitation, family = "poisson", data = modSel)
ab_by_temp_and_edge<-glm(observedAbundance ~ meanSummerTemp + totalEdge, family = "poisson", data = modSel)
ab_by_precip_and_edge<-glm(observedAbundance ~ summerPrecipitation + totalEdge, family = "poisson", data = modSel)
ab_by_temp<-glm(observedAbundance ~ meanSummerTemp, family = "poisson", data = modSel)
ab_by_precip<-glm(observedAbundance ~ summerPrecipitation, family = "poisson", data = modSel)
ab_by_edge<-glm(observedAbundance ~  totalEdge, family = "poisson", data = modSel)
ab_null<-glm(observedAbundance ~ 1, family = "poisson", data = modSel)
model.sel(global_mod, ab_by_temp_and_precip, ab_by_temp_and_edge, ab_by_precip_and_edge, ab_by_temp, ab_by_precip, ab_by_edge, ab_null)

summary(ab_by_temp_and_edge)

###the best model of abundance included both temperature and edge (ab_by_temp_and_edge), followed by the model that included all three variables (global_mod).
###The null model was the worst-performing model.

# Step 4: Interpret these results.
# Were your hypotheses supported? What is the relative importance of each predictor?
# What is your general conclusion?

###Since one of the top-ranked models (within 2 delta AIC of the top-performing model) contained all three variables, this supports the hypotheses that 
###all three variables predict the abundance of the species of interest.  
###Summer temperature was the best individual predictor; the model using only summer temperature to predict abundance (ab_by_temp) had the lowest AIC of all single-variable models.
###Amount of edge (total edge) was the second best-performing variable in its individual model (ab_by_edge).  

###My conclusion is that abundance of the species is best predicted by the mean summer temperature and the amount of edge in a given habitat.  Both variables are highly significant (p < 0.0001), 
###indicating that the correlation between abundance and either variable is extremely unlikely to have occurred under a null scenario.  Temperature is the single best predictor
###of abundance.  In the top-ranked model (ab_by_temp_and_edge), the effects of temperature and edge on abundance where quite large in magnitude; for every increase in temperature by one unit,
###abundance increased by 0.53 individuals.  Amount of edge had an influence almost equal in magnitude but opposite in effect; for every increase in one unit of edge, abundance
###of the species decreased by 0.55 individuals.  This suggests that, based on the variables measured, the habitats with the greatest abundance of the species would be those
###with relatively high summer temperatures but little amount of edge habitat.






