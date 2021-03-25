## Load some packages that will be useful
library(mgcv) # load the MGCV package with Simon Wood's gam() functions
library(readr) # Hadley Wickham's package for reading in data easily
library(ggplot2) # for plotting with ggplot
library(ggfortify) # for autoplot
library(dplyr)

##################################################################################################
## We'll first examine the bias-variance tradeoff in a simplified form with a small exercise.
## If you remember, this tradeoff is one of the concepts behind finding the optimal amount of smoothing.
## We'll fit linear regressions to parts of the data, increasing the number of parts from 1 to 10.
##################################################################################################

# First, read in the bioluminescence data.
# Sources is the number of bioluminescent plankton seen in each water sample
# SampleDepth is the depth of the water sample, in m
# Station is the ID of the location at which multiple water samples were taken
ISIT <- read_tsv(url('https://github.com/aeda2021/2021_master/raw/main/raw-data/ISIT.txt')) # tsv for tab-separated values

# Subset it to one station. This is what we'll work with for now.
ISITsub <- subset(ISIT, Station == 8)
head(ISITsub)

# Plot the data
ggplot(ISITsub, aes(x=SampleDepth, y=Sources)) +
	geom_point()

# Now you can fit a linear regression to the ISITsub dataset, with Sources as the response and SampleDepth as the explanatory variable
# Please call the output "mod1", as I've started to fill in for you
mod1 <- lm(Sources ~ SampleDepth, data = ISITsub)

# Q1: What was your code for fitting a linear regression of Sources vs. SampleDepth?
##lm(Sources ~ SampleDepth, data = ISITsub)

# We'll predict the mean response and confidence intervals from this model and save the output
ISITsub$mod1 <- predict(mod1)
ISITsub$mod1lwr <- predict(mod1, interval='confidence', level=0.95)[,'lwr']
ISITsub$mod1upr <- predict(mod1, interval='confidence', level=0.95)[,'upr']

# Now plot the linear model fit to the full dataset on top of the data
ggplot(ISITsub, aes(x=SampleDepth, y=Sources)) +
	geom_point() +
	geom_line(aes(y=mod1)) + 
	geom_ribbon(aes(ymin=mod1lwr, ymax=mod1upr), alpha=0.3)

# Q2: Use your skills from earlier in this course and evaluate this linear model (autoplot may be useful here). How does this fit look to you? Does a linear regression look reasonable? Any well-justified answer is appropriate. 
autoplot(mod1)
##Residuals vs. fitted shows that the residuals are not evenly distributed across x
##indicating the data don't conform to a line; the normal QQ plot shows pretty strong
##deviations from the line at low and high values, indicating the residuals are not 
##normally distributed (right?); scale-location also shows an uneven distribution of 
##variance across x; residuals vs. leverage shows that values with high residuals are 
##impacting the model

# For comparison, let's fit linear models to two halves of the data. This involves a fair bit of coding in R, so please ask for help understanding the code where things are confusing. 
# First, we have to find the halves of the data
quants2 <- quantile(ISITsub$SampleDepth, probs=seq(0, 1, by=0.5)) # define the ends and the half-way point in the data with the quantile function
ISITsub$split2 <- cut(ISITsub$SampleDepth, breaks=quants2, include.lowest=TRUE, labels=1:2) 
head(ISITsub)

# Q3: In your own words, what does the cut() function do? How are we using it here?
##the cut() function assigned data in the lower 50th percentile to one factor, and 
##the data in the upper 50th percentile to another factor, and stored these factor 
##levels in $split2

# Next, let's fit a linear model to each subset (half) of the data. We do this in a loop to make it easier.
mod2s <- vector('list', length=2) # A list to hold each of our models in a single, convenient object
ISITsub$mod2 <- ISITsub$mod2lwr <- ISITsub$mod2upr <- NA # Initialize the variables we'll use and fill with NA
for(i in 1:length(mod2s)){ # Loop through each model
	mod2s[[i]] <- lm(Sources ~ SampleDepth, data=ISITsub[ISITsub$split2==i,]) # Fit a linear regression to the appropriate subset of the data. Now see why we made the split2 vector?
	ISITsub$mod2[ISITsub$split2==i] <- predict(mod2s[[i]]) # Now predict from that model. Notice we're only predicting for part of the data.
	ISITsub$mod2lwr[ISITsub$split2==i] <- predict(mod2s[[i]], interval='confidence', level=0.95)[,'lwr'] # Same for the confidence intervals
	ISITsub$mod2upr[ISITsub$split2==i] <- predict(mod2s[[i]], interval='confidence', level=0.95)[,'upr']
}

# Plot the data, the 1-model fit, and the 2-model fit
# There will be an odd 3rd line segment connecting the end of the first model fit to the beginning of the second model fit. Just pretend it's not there.
ggplot(ISITsub, aes(x=SampleDepth, y=Sources)) +
	geom_point() +
	geom_line(aes(y=mod1)) +
	geom_ribbon(aes(ymin=mod1lwr, ymax=mod1upr), alpha=0.3) +
	geom_line(aes(y=mod2, color='red')) +
	geom_ribbon(aes(ymin=mod2lwr, ymax=mod2upr), alpha=0.3, fill='red')

# Q4: Has the fit between the model and the data improved? What's the downside of continuing the split the data into finer and finer chunks and fitting more models?
##It appears to have improved, yes.  autoplot() on mod2s gives two sets of plots (8 plots total) but the assumptions
##of normality, homogeneity of residuals across x, etc., appear less violated in each of the separate 'mini-models'

##a downside to splitting the data is that your sample size n for each model goes down with each split, so you'd quickly eat up your degrees of freedom and the 
##trustworthiness of each model.  It may also over-fit the model.

autoplot(mod2s)

# Now, it's your turn. Please fit 10 linear regression models. I'd recommend using the 2-model code above as a guide and tweaking it.
quants10 <- quantile(ISITsub$SampleDepth, probs=seq(from = 0, to = 1, by=0.1)) # define the ends and the half-way point in the data with the quantile function
ISITsub$split10 <- cut(ISITsub$SampleDepth, breaks=quants10, include.lowest=TRUE, labels=1:10) 
head(ISITsub)
glimpse(ISITsub$split10)
ISITsub$split10

mod10s <- vector('list', length=10) # A list to hold each of our models in a single, convenient object
ISITsub$mod10 <- ISITsub$mod10lwr <- ISITsub$mod10upr <- NA # Initialize the variables we'll use and fill with NA
for(i in 1:length(mod10s)){
    mod10s[[i]] <- lm(Sources ~ SampleDepth, data=ISITsub[ISITsub$split10==i,]) # Fit a linear regression to the appropriate subset of the data. Now see why we made the split2 vector?
    ISITsub$mod10[ISITsub$split10==i] <- predict(mod10s[[i]]) # Now predict from that model. Notice we're only predicting for part of the data.
    ISITsub$mod10lwr[ISITsub$split10==i] <- predict(mod10s[[i]], interval='confidence', level=0.95)[,'lwr'] # Same for the confidence intervals
    ISITsub$mod10upr[ISITsub$split10==i] <- predict(mod10s[[i]], interval='confidence', level=0.95)[,'upr']
}

# Now plot the 10 models on top of the data and on top of the 1- and 2-model fits. The plot with the data, 1-model, and 2-model fit is probably a useful guide here.

ggplot(ISITsub, aes(x=SampleDepth, y=Sources)) +
    geom_point() +
    geom_line(aes(y=mod1)) +
    geom_ribbon(aes(ymin=mod1lwr, ymax=mod1upr), alpha=0.3) +
    geom_line(aes(y=mod2, color='red')) +
    geom_ribbon(aes(ymin=mod2lwr, ymax=mod2upr), alpha=0.3, fill='red')+
    geom_line(aes(y = mod10))+
    geom_ribbon(aes(ymin=mod10lwr, ymax=mod10upr), alpha=0.3, fill='green')

# Q5: Compare your 10-model fit to the 1-model fit. Which model's predictions are furthest from the observed data? Which model has the widest confidence bounds? How does this (or does this not) illustrate the bias-variance tradeoff?

##I am not sure how to evaluate model fit empirically, because my usual routes (AIC(), model.sel(), summary()) don't recognize mod10s as a single model object.
##visually, the mod10s set has huge errors due to the small sample sizes in each model, and lots of mini-trends all over the place that likely don't represent 
##the underlying relationship with depth.  Model 2 looks much more representative.


##



####################################################
## Now you'll fit some GAMs and evaluate your models
####################################################
#install.packages("maps") # if needed
library(maps) # has map data in it

# load the data
spdata <- readRDS(url('https://github.com/aeda2021/2021_master/raw/main/raw-data/gadusmorhua.rds')) # load the spdata data.frame with abundance and environmental data from cod surveys
spdata$presfit01 <- as.numeric(spdata$presfit) # make a vector that's nice for plotting
spdata <- spdata[order(spdata$presfit01),] # order the data from absent to present for ease of plotting

# examine the data
head(spdata) # look at the dataset
summary(spdata)

# make a map of the data
world <- map_data('world')

ggplot() + 
	geom_polygon(data=world, aes(x=long, y=lat, group=group), color='black', fill=NA) + # the map
	xlim(-100, -45) + 
	ylim(23, 62) +
	geom_point(data=spdata, aes(x=lon, y=lat, color=presfit, alpha=0.1), size=0.01) # the data points. note that because the dataframe is sorted from absent to present, the presences are plotted on top of the absences


# Q6: Based on the map, do you think cod prefer warmer or cooler waters? Why do you think that?
##Cod prefer cooler waters; presence points are all at northern latitudes (absence points are everywhere)

# Q7: Plot cod presence/absence (the presfit vector) vs. the bottom temperature (SBT.actual). At what range of temperatures have cod been observed?
ggplot() +
    geom_point(data = spdata, aes(x=SBT.actual, y = presfit01))

# Q8: Fit a GAM for presfit against SBT.actual. Check the model. Does it look like the assumptions have been met? Why or why not?
gam1 <- gam(presfit01 ~ SBT.actual, family = gaussian, data = spdata)
gam.check(gam1)

# Q9: Fit a new GAM with a more appropriate error structure and save it as "mod2". The gam() function takes the same family= argument as does glm(). Which error structure and link function did you choose?
mod2<- gam(presfit01 ~ SBT.actual, family = binomial, link = "logit", data = spdata)
##I used a binomial error structure and the logit link function.

# Q10: Check your mod2 GAM. Does it look like the assumptions have been met? Make sure to read the text output as well as look at the graphs. If something doesn't look right, what would you do to fix it?

gam.check(mod2)

##Some assumptions are better met by mod2, but the histogram of residuals shows that the residuals are not normally distributed (not even close).
##The QQ plot looks pretty good.

# Q11: Interpret your mod2 GAM (summary function). How much deviance is explained? Is the SBT.actual term significant at alpha=0.05? How wiggly do you expect the smooth fit to be?

summary(mod2)

##Only 23% of the variation is explained by the GAM.  SBT.actual is significant.  Because it's a binomial regression, I don't think it will be very wiggly, but there is no edf in the summary.

# Now let's make predictions from our last model (mod2) and plot them.
nd <- data.frame(SBT.actual = seq(-5,30, by=0.5)) # make a data.frame of new explanatory variables
nd$mod2 <- predict(mod2, newdata=nd, type='response') # predict on the scale of the response
nd$mod2se <- predict(mod2, newdata=nd, type='response', se.fit=TRUE)$se.fit # get the standard errors of the fit

ggplot(spdata, aes(x=SBT.actual, y=presfit01)) +
	geom_point() +
	geom_line(data=nd, aes(x=SBT.actual, y=mod2, color='red')) +
	geom_ribbon(data=nd, aes(x=SBT.actual, ymin=mod2-mod2se, ymax=mod2+mod2se), alpha=0.3, fill='red', inherit.aes=FALSE)



# Q12: Does the fit look realistic? Why or why not? Does it look overfit? Why or why not?

###I don't think it is overfit.  Even where cod are detected presence is predicted to be very low, for example between 5 and 15 degrees.  Perhaps it is an under-fit model.


# Q13: This dataset is actually a concatenation of seven different surveys which each have slightly different abilities to catch cod. Fit a new GAM that includes a categorical predictor for survey (the region vector). Based on AIC, which model would you choose? How confident would you be?
mod3<- gam(presfit01 ~ SBT.actual + region, family = binomial, link = "logit", data = spdata)
summary(mod3)

AIC(mod2, mod3)

##including sampling technique as an effect improved the deviance explained to 31.8%, and AIC was better than mod2 (by a whole lot).