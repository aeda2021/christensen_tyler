rm(list = ls())

setwd("C:/Users/piedm/Dropbox/R/LEOW_roost_site_selection")
rawdata<-read.csv("2021_Roost_Data_v3.csv", fileEncoding="UTF-8-BOM") ##the "fileEncoding" eliminated a strange "ï.." in the first column header RoostID. Apparently a common problem with files generated from excel.

library(dplyr)
library(ggplot2)
library(ggfortify)
library(MuMIn)
library(lme4)
library(glmmTMB)
library(DHARMa)
library(AED)
names(rawdata)
roostdata<-select(rawdata, c(1,2,3,9,15,16,17,18,25,32,37,43,48,53,58,63,68))
glimpse(roostdata)
nrow(roostdata)
roostdata$RoostStatus <- as.factor(roostdata$RoostStatus)
glimpse(roostdata)

##boxplots to visualize data

names(roostdata)

ggplot(data = roostdata, aes(x = RoostStatus, y = CanopyHeight))+
    geom_boxplot()

ggplot(data = roostdata, aes(x = RoostStatus, y = CanopyClosure))+
    geom_boxplot()

ggplot(data = roostdata, aes(x = RoostStatus, y = StemsUnder2.5))+
    geom_boxplot()

ggplot(data = roostdata, aes(x = RoostStatus, y = StemsOver2.5))+
    geom_boxplot()

ggplot(data = roostdata, aes(x = RoostStatus, y = Stems))+
    geom_boxplot()

ggplot(data = roostdata, aes(x = RoostStatus, y = EvergreenClosure))+
    geom_boxplot()

ggplot(data = roostdata, aes(x = RoostStatus, y = InvasiveClosure))+
    geom_boxplot()

ggplot(data = roostdata, aes(x = RoostStatus, y = RoostCanopyClosure))+
    geom_boxplot()

ggplot(data = roostdata, aes(x = RoostStatus, y = RoostVineClosure))+
    geom_boxplot()

ggplot(data = roostdata, aes(x = RoostStatus, y = RoostHeightAve))+
    geom_boxplot()

ggplot(data = roostdata, aes(x = RoostStatus, y = RoostTreeHeightAve))+
    geom_boxplot()

ggplot(data = roostdata, aes(x = RoostStatus, y = CanopyHeightRoostAve))+
    geom_boxplot()

ggplot(data = roostdata, aes(x = RoostStatus, y = RoostTreeDBHAve))+
    geom_boxplot()

ggplot(data = roostdata, aes(x = RoostStatus, y = DistanceToTreeAve))+
    geom_boxplot()

###Look for outliers
op <- par(mfrow = c(7,2), mar = c(2,4,2,2))
dotchart(roostdata$CanopyHeight, main = "Can. Height")
dotchart(roostdata$CanopyClosure, main = "Can. Closure")
dotchart(roostdata$StemsUnder2.5, main = "# Stems Under 2.5cm")
dotchart(roostdata$StemsOver2.5, main = "# Stems Over 2.5cm")
dotchart(roostdata$Stems, main = "# Stems")
dotchart(roostdata$EvergreenClosure, main = "Evergreen Closure")
dotchart(roostdata$InvasiveClosure, main = "Invasive Closure")
dotchart(roostdata$RoostCanopyClosure, main = "Roost Can. Closure")
dotchart(roostdata$RoostVineClosure, main = "Roost Vine Closure")
dotchart(roostdata$RoostHeightAve, main = "Roost Height Ave")
dotchart(roostdata$RoostTreeHeightAve, main = "Roost Tree Height")
dotchart(roostdata$CanopyHeightRoostAve, main = "Canopy Height @ Roost")
dotchart(roostdata$RoostTreeDBHAve, main = "Roost Tree DBH")
dotchart(roostdata$DistanceToTreeAve, main = "Dist. to Nearest Tree")

##Note: Stems over 2.5cm might be concerning.  Distance to nearest tree definitely has a concerning outlier.

###Clean data... remove NA values (except from RoostHeightAve, which will be analyzed separately later) and impute missing values with mean for variable (there is only one NA)

roostdata$CanopyHeight[is.na(roostdata$CanopyHeight)] = mean(roostdata$CanopyHeight, na.rm=TRUE)

glimpse(roostdata)


##scale and center variables; as.numeric() helped later with generating probability plots
roostdata$CanopyHeight<-as.numeric(scale(roostdata$CanopyHeight, center = TRUE, scale = TRUE))
roostdata$CanopyClosure<-as.numeric(scale(roostdata$CanopyClosure, center = TRUE, scale = TRUE))
roostdata$StemsUnder2.5<-as.numeric(scale(roostdata$StemsUnder2.5, center = TRUE, scale = TRUE))
roostdata$StemsOver2.5<-as.numeric(scale(roostdata$StemsOver2.5, center = TRUE, scale = TRUE))
roostdata$Stems<-as.numeric(scale(roostdata$Stems, center = TRUE, scale = TRUE))
roostdata$EvergreenClosure<-as.numeric(scale(roostdata$EvergreenClosure, center = TRUE, scale = TRUE))
roostdata$InvasiveClosure<-as.numeric(scale(roostdata$InvasiveClosure, center = TRUE, scale = TRUE))
roostdata$RoostVineClosure<-as.numeric(scale(roostdata$RoostVineClosure, center = TRUE, scale = TRUE))
roostdata$RoostTreeHeightAve<-as.numeric(scale(roostdata$RoostTreeHeightAve, center = TRUE, scale = TRUE))
roostdata$RoostCanopyClosure<- as.numeric(scale(roostdata$RoostCanopyClosure, center = TRUE, scale = TRUE))
roostdata$CanopyHeightRoostAve<-as.numeric(scale(roostdata$CanopyHeightRoostAve, center = TRUE, scale = TRUE))
roostdata$RoostTreeDBHAve<-as.numeric(scale(roostdata$RoostTreeDBHAve, center = TRUE, scale = TRUE))
roostdata$DistanceToTreeAve<-as.numeric(scale(roostdata$DistanceToTreeAve, center = TRUE, scale = TRUE))

glimpse(roostdata)

##Collinearity and correlation

dim(roostdata)

##pairs plots to assess which variables may have colinearity
pairs(roostdata[,4:17])
##(some definitely do; Stems vs. StemsUnder2.5, CanopyClosure vs. EvergreenClosure, RoostTreeHeight vs. CanopyHeightRoost, 
##CanopyHeight vs. CanopyHeightRoost
cor(roostdata[,4:17])
##using cor() to calculate correlation coefficients confirmed that the following pairs were correlated (coeff. > 0.7):
######CanopyHeight vs. CanopyHeightRoostAve
######CanopyCLosure vs. EvergreenClosure
######Stems vs. StemsUnder2.5
######RoostTreeHeight vs. CanopyHeightRoostAve


##FACTOR SELECTION

##DistanceToTreeAve has significant outlier; checking if univariate model is meaningful by eliminating outlier and seeing if glm indicates relationship
no_outliers<-roostdata[-24,]
plot(x = no_outliers$RoostStatus, y = no_outliers$DistanceToTreeAve)
##Distributions appear similar... eliminate DistanceToTreeAve

##Elimination of factors based on similarity of distributions among "levels" (RoostStatus 0 vs 1):

##eliminate DistanceToTreeAve
##eliminate RoostTreeHeightAve

##Elimination of factors with high collinearity (cor > 0.7). 
##CanopyHeight vs. CanopyHeightRoostAve

CH <- glm(RoostStatus ~ CanopyHeight, family = "binomial", data = roostdata)
CHR <- glm(RoostStatus ~ CanopyHeightRoostAve, family = "binomial", data = roostdata)
model.sel(CH, CHR) ##keep CanopyHeight, eliminate CanopyHeightRoostAve

##CanopyClosure vs. EvergreenClosure
##keep EvergreenClosure, since CanopyClosure and RoostCanopyClosure capture similar information, and RoostCanopyClosure is retained

##Elimination of factors for other reasons:

##Eliminate Stems to avoid the partial redundancy between Stems and StemsOver2.5 / StemsUnder2.5.  Keep StemsOver and StemsUnder.
##Eliminate InvasiveClosure; while interesting, owls probably select diurnal roost habitats on the basis
##of vegetation structure, not species composition.  Though the two are related, structural differences should 
##be captured in the other variables.

##thinning to 'kept' variables
names(roostdata)
roostdata <- select(roostdata, c(1,2,3,4,6,7,9,11,12,16))
names(roostdata)
glimpse(roostdata)

#######MODEL SELECTION########

##started with global model, eliminated variables with the highest individual AIC scores until glm would fit without overfitting

nullmod <- glm(RoostStatus ~ 1, family = "binomial", data = roostdata)
AIC(nullmod)

globmod <- glm(RoostStatus ~ CanopyHeight + StemsUnder2.5 + StemsOver2.5 + EvergreenClosure + RoostCanopyClosure + RoostVineClosure + RoostTreeDBHAve, family = "binomial", data = roostdata)
AIC(globmod)

step(globmod)

##top-ranked model is RoostStatus ~ EvergreenClosure + RoostCanopyClosure + RoostVineClosure + RoostTreeDBH
final_mod <- glm(RoostStatus ~ EvergreenClosure + RoostCanopyClosure + RoostVineClosure + RoostTreeDBHAve, family = "binomial", data = roostdata)
model.sel(nullmod, final_mod)

##Simulated residuals using DHARMa
##evaluating normality of residuals:

res <- simulateResiduals(fittedModel = final_mod, plot = T)

plotResiduals(res, roostdata$EvergreenClosure)
plotResiduals(res, roostdata$RoostCanopyClosure)
plotResiduals(res, roostdata$RoostVineClosure)
plotResiduals(res, roostdata$RoostTreeDBHAve)

##re-evaluating collinearity using variable inflation factors:

library(car)
vif(final_mod)

##VIF scores are all < 4

###next selecting only occupied roosts to generate some informative plots

roost_trees <- rawdata %>% filter(RoostStatus == 1)
control_trees <- rawdata %>% filter(RoostStatus == 0)

plot(roost_trees$RoostVineClosure, roost_trees$EvergreenClosure)
abline(lm(RoostVineClosure ~ EvergreenClosure, data = roost_trees), col = "blue")

##Apparently as evergreen closure IN THE PLOT goes down, vine closure AT THE ROOST goes up.

mean(roost_trees$EvergreenClosure)
mean(control_trees$EvergreenClosure)

##Even at occupied roosts evergreen closure was low (<50%)

###################PROBABILITY PLOTS##########################

#y = RoostStatus
#x = RoostCanopyClosure, etc.

##x new data frame and x axis (0 - 100% but with 26 breaks[nrow])
df.RCC <-data.frame(x = seq(0,100,length.out = (length(rawdata$RoostID))))
head(df.RCC)

##Fit the univariate GLM:
RCC <- glm(RoostStatus ~ RoostCanopyClosure, family = "binomial", data = rawdata)

##predicted values (converted from log odds to probability), added to data frame as column "logit"
df.RCC$logit <- predict(RCC, list(RoostCanopyClosure = df.RCC$x), se.fit = TRUE, type = "response")$fit ###make sure RoostCanopyClosure matches name/spelling in the model
head(df.RCC)

##standard error of predicted values, added to data frame as column "pse_logit"
df.RCC$pse_logit <- predict(RCC, list(RoostCanopyClosure = df.RCC$x), se.fit = TRUE, type = "response")$se.fit
head(df.RCC)

##predicted values plus and minus 1.96 * standard error, added to data frame
df.RCC$pupper_logit <- df.RCC$logit + (1.96 * df.RCC$pse_logit)  # 95% CI upper bound
df.RCC$plower_logit <- df.RCC$logit - (1.96 * df.RCC$pse_logit)  # 95% CI lower bound

##plot canvas; RoostStatus by predictor
workaround<-(as.numeric(rawdata$RoostStatus))-1 ###This workaround was because plotting RoostStatus in base r, 
##the y-axis was automatically going from 1 to 2 rather than 0 to 1.  
##Instead I set rawdata$RoostStatus asnumeric() and then subtracted 1.
##I did not seem to have this problem in ggplot.

plot(rawdata$RoostCanopyClosure, rawdata$RoostStatus, xlim = c(0,100), xlab = "Canopy Closure (%)", ylab = "Probability of Use")
head(rawdata$RoostStatus)

##lines; values 0 - 100 (974 divisions) by predicted "logit" values
lines (df.RCC$x, df.RCC$logit)

##lines; y (logit value) ~ x (0 - 100% CWD), upper and lower 95% CI
lines(df.RCC$pupper_logit ~ df.RCC$x, type = "l", lty = 2)
lines(df.RCC$plower_logit ~ df.RCC$x, type = "l", lty = 2)

###trying again with evergreen

EC <- glm(RoostStatus ~ EvergreenClosure, family = "binomial", data = rawdata)

df.EC <-data.frame(x = seq(0,100,length.out = length(nrow(rawdata))))
summary(EC)

##x new data frame and x axis (0 - 100% but with 26 breaks[nrow])
EC.df <-data.frame(x = seq(0,100,length.out = (length(rawdata$RoostID))))
head(EC.df)

##predicted values (converted from log odds to probability), added to data frame as column "logit"
EC.df$logit <- predict(EC, list(EvergreenClosure = EC.df$x), se.fit = TRUE, type = "response")$fit 

##standard error of predicted values, added to data frame as column "pse_logit"
EC.df$pse_logit <- predict(EC, list(EvergreenClosure = EC.df$x), se.fit = TRUE, type = "response")$se.fit
head(EC.df)

##predicted values plus and minus 1.96 * standard error, added to data frame
EC.df$pupper_logit <- EC.df$logit + (1.96 * EC.df$pse_logit)  # 95% CI upper bound
EC.df$plower_logit <- EC.df$logit - (1.96 * EC.df$pse_logit)  # 95% CI lower bound

plot(rawdata$EvergreenClosure, rawdata$RoostStatus, xlab = "Evergreen Closure (%)", ylab = "Probability of Use")
head(rawdata$EvergreenClosure)

##lines; values 0 - 100 (974 divisions) by predicted "logit" values
lines (EC.df$x, EC.df$logit)

##lines; y (logit value) ~ x (0 - 100% CWD), upper and lower 95% CI
lines(EC.df$pupper_logit ~ EC.df$x, type = "l", lty = 2)
lines(EC.df$plower_logit ~ EC.df$x, type = "l", lty = 2)

###Again with vine
VC <- glm(RoostStatus ~ RoostVineClosure, family = "binomial", data = rawdata)

df.VC <-data.frame(x = seq(0,100,length.out = length(nrow(rawdata))))
summary(VC)
VC.df <-data.frame(x = seq(0,100,length.out = (length(rawdata$RoostID))))
head(VC.df)

VC.df$logit <- predict(VC, list(RoostVineClosure = VC.df$x), se.fit = TRUE, type = "response")$fit 

VC.df$pse_logit <- predict(VC, list(RoostVineClosure = VC.df$x), se.fit = TRUE, type = "response")$se.fit
head(VC.df)

VC.df$pupper_logit <- VC.df$logit + (1.96 * VC.df$pse_logit)  # 95% CI upper bound
VC.df$plower_logit <- VC.df$logit - (1.96 * VC.df$pse_logit)  # 95% CI lower bound

###restricts values to between 0 and 1 (for the probability plots)

VC.df$pupper_logit_cor <- ifelse(VC.df$pupper_logit > 1, "1", ##if values are greater than one make them 1
    ifelse(VC.df$pupper_logit < 0, "0",  ##if they are not greater than one, THEN if values are less than zero, make them 0
    VC.df$pupper_logit)) %>% ##and if they are not less than zero, make them equal to the value of pupper
    as.numeric(VC.df$pupper_logit_cor) ##makes it into numeric vector, for GGplot

VC.df$plower_logit_cor <- ifelse(VC.df$plower_logit > 1, "1", ##if values are greater than one make them 1
    ifelse(VC.df$plower_logit < 0, "0",  ##if they are not greater than one, THEN if values are less than zero, make them 0
    VC.df$plower_logit)) %>% ##and if they are not less than zero, make them equal to the value of pupper
    as.numeric(VC.df$plower_logit_cor) ##makes it into numeric vector, for GGplot

glimpse(VC.df)

###plotting RoostVineClosure in base R    
plot(rawdata$RoostVineClosure, rawdata$RoostStatus, xlab = "Percent Vine (%)", ylab = "Probability of Use")
head(rawdata$RoostVineClosure)

lines (VC.df$x, VC.df$logit)

lines(VC.df$pupper_logit_cor ~ VC.df$x, type = "l", lty = 2)
lines(VC.df$plower_logit_cor ~ VC.df$x, type = "l", lty = 2)

###now trying with ggplot and using errors between 0 and 1
ggplot(data = rawdata, aes(x = RoostVineClosure, y = logit)) + ##good
    theme_bw() +
    geom_ribbon(data = VC.df, aes(x = x, ymin = plower_logit_cor, ymax = pupper_logit_cor), fill = "grey80") + ## GOOD
    geom_line(data = VC.df, aes(x = x, y = logit), size = 1.2, colour = "blue")##GOOD

summary(rawdata$RoostVineClosure)

#geom_point() ## add to top line of ggplot for binomial values
        
###Clean version of doing egergreen closure probability plot and 95% CI for mean in ggplot; use as template for the rest
EC <- glm(RoostStatus ~ EvergreenClosure, family = "binomial", data = rawdata)

df.EC <-data.frame(x = seq(0,100,length.out = length(nrow(rawdata))))
summary(EC)
EC.df <-data.frame(x = seq(0,100,length.out = (length(rawdata$RoostID))))
head(EC.df)

EC.df$logit <- predict(EC, list(EvergreenClosure = EC.df$x), se.fit = TRUE, type = "response")$fit 

EC.df$pse_logit <- predict(EC, list(EvergreenClosure = EC.df$x), se.fit = TRUE, type = "response")$se.fit
head(EC.df)

EC.df$pupper_logit <- EC.df$logit + (1.96 * EC.df$pse_logit)  # 95% CI upper bound
EC.df$plower_logit <- EC.df$logit - (1.96 * EC.df$pse_logit)  # 95% CI lower bound

EC.df$pupper_logit_cor <- ifelse(EC.df$pupper_logit > 1, "1", ##if values are greater than one make them 1
    ifelse(EC.df$pupper_logit < 0, "0",  ##if they are not greater than one, THEN if values are less than zero, make them 0
    EC.df$pupper_logit)) %>% ##and if they are not less than zero, make them equal to the value of pupper
    as.numeric(EC.df$pupper_logit_cor) ##makes it into numeric vector, for GGplot

EC.df$plower_logit_cor <- ifelse(EC.df$plower_logit > 1, "1", ##if values are greater than one make them 1
    ifelse(EC.df$plower_logit < 0, "0",  ##if they are not greater than one, THEN if values are less than zero, make them 0
    EC.df$plower_logit)) %>% ##and if they are not less than zero, make them equal to the value of pupper
    as.numeric(EC.df$plower_logit_cor) ##makes it into numeric vector, for GGplot

ggplot(data = rawdata, aes(x = EvergreenClosure, y = logit)) + ##good
    theme_bw() +
    geom_ribbon(data = EC.df, aes(x = x, ymin = plower_logit_cor, ymax = pupper_logit_cor), fill = "grey80") + ## GOOD
    geom_line(data = EC.df, aes(x = x, y = logit), size = 1.2, colour = "blue")##GOOD