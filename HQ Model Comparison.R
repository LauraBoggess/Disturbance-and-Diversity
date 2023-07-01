# Multiple Regression and Model Building for HQ Paper
# Laura Boggess, 28 June, 2023

#Set working directory
setwd("~/Dropbox/My Mac (mhu19049-MacBook Pro)/Desktop/HQAnalysis")
#load data
HQFG <- read.csv("HQFG.csv")
HQMR <- read.csv("HQMR.csv")
View(HQFG)
View(HQMR)

# Examine the dependent variable
summary(HQMR)
hist(HQMR$HQ, breaks=20)

# Create alternative models for Model Selection

m0 <- lm(LS ~ Lat + Long + Slope..degrees. + Elevation..m. + HQ, 
         data=HQMR)	# full model
m1 <- lm(LS ~ Lat + Long + Slope..degrees. + Elevation..m., data=HQMR)
m2 <- lm(LS ~ Lat + Long + Slope..degrees., data=HQMR)
m3 <- lm(LS ~ Lat + Long, data=HQMR)
m4 <- lm(LS ~ Lat*Long + HQ, data=HQMR)
m5 <- lm(LS ~ Long + HQ, data=HQMR)
m6 <- lm(LS ~ Slope..degrees. + HQ, data=HQMR)	
m7 <- lm(LS ~ Elevation..m. + HQ, data=HQMR)
m8 <- lm(LS ~ HQ, data=HQMR)
m9 <- lm(LS ~ Long + Slope..degrees. + HQ, 
         data=HQMR)
m10 <- lm(LS ~ Lat + Slope..degrees. + HQ, 
          data=HQMR)
m11 <- lm(LS ~ Lat + Elevation..m. + HQ, 
          data=HQMR)
m12 <- lm(LS ~ Long + Elevation..m. + HQ, 
          data=HQMR)
m13 <- lm(LS ~ Long + Elevation..m., data=HQMR) # model without HQ   

# SS and P value show how strong effect of HQ is (just FYI)
anova(m12, m13)            

# compare the full model to m12; hoping for large P here.
anova(m12,m0)             

# residuals after correcting for longitude + elevation
m13.resid <- residuals(m13) 

# visualize HQ effect
plot(x=HQMR$HQ, y=m13.resid, xlab=HQ, ylab=residuals(Long,elev)) 
x <- HQMR$HQ
length(x)
length(m13.resid)
plot(x,m13.resid)

# Compute table of AIC metrics
aics <- AIC(m0,m1,m2,m3,m4,m5,m6, m7,m8,m9,m10,m11,m12,m13)
aics$model <- row.names(aics)
row.names(aics) <- NULL
aics<-aics[order(aics$AIC),]	# sort by AIC value
for(i in 1:nrow(aics)){ aics$diff[i]<- aics$AIC[i] - aics$AIC[1] }
aics$wi<-2.71828182845904523536^(-0.5*aics$diff)
aics$aic.weights<-aics$wi/sum(aics$wi)
aics[,c("model","df","AIC", "diff", "aic.weights")] 



# Backwards stepwise regression; compare results with top model from Model Selection
library(MASS)
m.step <- stepAIC(m0)
summary(m.step)

# Account for longitude
M <- lm(LS ~ Long, data = HQMR)
long.resid <- residuals(M)
M.HQ <- lm(long.resid ~ HQ, data = HQMR)
