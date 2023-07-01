install.packages("rmarkdown")
install.packages("knitr")
install.packages("ggplot2")

setwd("~/Dropbox/My Mac (mhu19049-MacBook Pro)/Desktop/HQAnalysis")
#load data
HQFG <- read.csv("HQFG.csv")
HQMR <- read.csv("HQMR.csv")
View(HQFG)   

# Multiple Regression to determine most important abiotic factor driving lichen richness -----------------------------------------------------

#Multiple Regression Model

#Check column names
names(HQMR)
#Correlation matrix
cor(HQMR)
cor(HQMR$LS, HQMR)
#Visualize that HQ and Long aren't correlated
plot(HQMR$Long,HQMR$HQ)
#Visualize that HQ and Elevation aren't correlated
plot(HQMR$Elevation..m.,HQMR$HQ)

lm(HQ ~ LS, data = HQMR)

# Original Regression Model -----------------------------------------------
#Make a blank regression model
mod = lm(LS ~ 1, HQMR)
summary(mod)
#Use add1() to choose the first variable
add1(mod, HQMR, test = "F")
#HQ is lowest AIC and most significant, so replace the intercept with the HQ variable
mod = lm(LS ~ HQ, HQMR)
#Run the add1() command again
add1(mod, scope = HQMR, test = "F")
#Longitude is significant and has the lowest AIC of the remaining variables
mod = lm(LS ~ HQ + Long, HQMR)
summary(mod)



# Linear Regression: Total Lichen Diversity and HQ ------------------------
# Run for all functional groups, just change response variable
M <- lm(LS ~ Long + Elevation..m., data = HQMR)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "Lichen Species Richness", data = HQFG)
abline(M.HQ)
confint(HQFG, 'M.HQ', level=0.95)

# Formula -----------------------------------------------------------------
# to add formula to plot
text(locator(1), "y = 0.87x + 20.588")
#Add R2     
text(locator(1), "R2 = .359")
str(HQCrustRegression)

# Original Model Names ----------------------------------------------------
#HQLS: Total Lichen Richness
#HQSR: Sexually Reproducing Richness
#HQAR: Asexually Reproducing Richness
#HQCort: Corticolous Richness
#HQSax: Saxicolous (rock-dwelling) Richness
#HQNCRock: Non-calcareous Rock Richness
#HQTerr: Terricolous (ground-dwelling) Richness
#HQCrustRegression: Crustose Richness
#HQFrut: Fruticose Richness
#HQFol: Foliose Richness
#HQSquam: Squamulose Richness
#HQTrent: Trentepohlia photobiont Richness
#HQCocc: Coccoid Algae photobiont Richness
#HQEMYes: Epiphytic Macrolichen Richness
#HQEMNo: Non-epihpytic macrolichen Richness
#HQNarrow: Narrow lobed Richness
#HQWide: Wide-lobed Richness
#HQPruinaYes: Pruina present Richness
#HQPruinaNo: Pruina absent Richness
#HQChemYes: Chemistry present Richness
#HQChemNo:Chemistry absent Richness

# Repeat for each dependent variable: Growth Form (crustose, fruticose, foliose, squamulose)


# Growth Form: Crustose --------------------------------------------------
M <- lm(HQFG$Crustose ~ HQMR$Long + HQMR$Elevation..m.)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "Crustose Species Richness", col = "red")
abline(M.HQ)

# Poisson Regression: Crustose ---------------------------------------------
#Compare to Poisson
#Make a poisson regression model for crustose species richness and HQ
names(HQFG)
crust.pois <- glm(formula = Crustose ~ HQ, family = "poisson", data = HQFG)
summary(crust.pois)

#create scatterplot
plot(Crustose ~ HQ, xlab = "Habitat Quality Score", ylab = "Crustose Species Richness", col = "orange", data = HQFG)
#add trendline
abline(crust.pois)
lpt <- cbind(x=HQFG$HQ, y=crust.pois$fitted.values)
lpt <- lpt[order(lpt$x),]
lines(lpt)
lines(x=HQFG$HQ, y=crust.pois$fitted.values)
text(locator(1), "y = 0.01x + 2.797")

#or in ggplot
ggplot(HQFG,aes(x=HQ,y=Crustose)) + geom_point() + geom_smooth() + theme()


# Growth Form: Fruticose --------------------------------------------
M <- lm(HQFG$Fruticose ~ HQMR$Long + HQMR$Elevation..m.)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "Fruticose Species Richness", col = "blue")
abline(M.HQ)

# Growth Form: Foliose -----------------------------------------

M <- lm(HQFG$Foliose ~ HQMR$Long + HQMR$Elevation..m.)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "Foliose Species Richness", col = "orange")
abline(M.HQ)

# Growth Form: Squamulose ------------------------------------------
M <- lm(HQFG$Squam ~ HQMR$Long + HQMR$Elevation..m.)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "Squamulose Species Richness", col = "purple")
abline(M.HQ)

# Reproductive Mode: Sexual------------------------------------------------
M <- lm(HQFG$Sexual ~ HQMR$Long + HQMR$Elevation..m.)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "Sexually Reproducing Species Richness", col = "blue")
abline(M.HQ)

# Reproductive Mode: Asexual Species ----------------------------------------------
M <- lm(HQFG$X...Asexual ~ HQMR$Long + HQMR$Elevation..m.)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "Asexually Reproducing Species Richness", col = "green")
abline(M.HQ)

# Photobiont: Trentepohlia -----------------------------------------
M <- lm(HQFG$PB.Green.trent. ~ HQMR$Long + HQMR$Elevation..m.)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "Trentepholiod Species Richness", col = "green")
abline(M.HQ)

# Photobiont: Coccoid Algae Photobiont ---------------------------
M <- lm(HQFG$PB.Green.coccoid ~ HQMR$Long + HQMR$Elevation..m.)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "Coccoid Species Richness", col = "blue")
abline(M.HQ)

# Photobiont: Cyanobacteria ---------------------------------------
M <- lm(HQFG$PB.Cyanobact. ~ HQMR$Long + HQMR$Elevation..m.)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "Cyanolichen Species Richness", col = "orange")
abline(M.HQ)

# Photobiont: Absent ------------------------------------------------
M <- lm(HQFG$PB.Absent ~ HQMR$Long + HQMR$Elevation..m.)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "No Photobiont Species Richness", col = "purple")
abline(M.HQ)
# Non-calcareous Rock -----------------------------------------------------
M <- lm(HQFG$Non.calc..rock ~ HQMR$Long + HQMR$Elevation..m.)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "Saxicolous Species Richness", col = "purple")
abline(M.HQ)

# Bark-Dwelling ------------------------------------------------
M <- lm(HQFG$Bark ~ HQMR$Long + HQMR$Elevation..m.)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "Corticolous Species Richness", col = "green")
abline(M.HQ)
# Soil-Dwelling ------------------------------------------------
M <- lm(HQFG$X..Humus ~ HQMR$Long + HQMR$Elevation..m.)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "Soil-dwelling Species Richness", col = "red")
abline(M.HQ)
# Pruina present  -------------------------------------------------------
M <- lm(HQFG$X..Pruina.Pres ~ HQMR$Long + HQMR$Elevation..m.)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "Pruinose Species Richness", col = "green")
abline(M.HQ)


# Pruina absent  ----------------------------------------------------------
M <- lm(HQFG$X..Pruina.Abs ~ HQMR$Long + HQMR$Elevation..m.)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "Non-Pruinose Species Richness", col = "blue")
abline(M.HQ)

# Cortex Chemistry Present ------------------------------------------------
M <- lm(HQFG$X..Cort.Chem.Present ~ HQMR$Long + HQMR$Elevation..m.)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "Cortex Chemistry Present Species Richness", col = "green")
abline(M.HQ)
# Cortex Chemistry Absent ------------------------------------------------
M <- lm(HQFG$X..Cor.Chem.Absent ~ HQMR$Long + HQMR$Elevation..m.)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "Cortex Chemistry Absent Species Richness", col = "blue")
abline(M.HQ)
# Epiphytic Macrolichen ---------------------------------------------------
M <- lm(HQFG$X..EpiphyticMacrolichen.Yes ~ HQMR$Long + HQMR$Elevation..m.)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "Epiphytic Macrolichen Species Richness", col = "blue")
abline(M.HQ)

# Non-epiphytic Macrolichen ---------------------------------------------------
M <- lm(HQFG$X..EpiphyticMacrolichen.No ~ HQMR$Long + HQMR$Elevation..m.)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "Non-epiphytic Macrolichen Species Richness", col = "green")
abline(M.HQ)

# Lobe Width: Narrow ------------------------------------------------
M <- lm(HQFG$X..Narrow.Lobes ~ HQMR$Long + HQMR$Elevation..m.)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "Narrow-lobed Species Richness", col = "blue")
abline(M.HQ)

# Lobe width: Broad ------------------------------------------------
M <- lm(HQFG$X..Broad.Lobes ~ HQMR$Long + HQMR$Elevation..m.)
M.resid <- residuals(M)
M.HQ <- lm(M.resid ~ HQ, data = HQMR)
plot(HQFG$HQ, M.resid)
summary(M.HQ)
plot(HQFG$HQ, M.resid, xlab = "Habitat Quality Score", ylab = "Broad-lobed Species Richness", col = "green")
abline(M.HQ)
# Poisson Regression: Reproductive mode ---------------------------------------------------

#Make a poisson regression model for sexual species and HQ
names(HQFG)
SexualRep.pois <- glm(formula = Sexual ~ HQ, family = "poisson", data = HQFG)
summary(lmsr.pois)
#or?
HQSL.glm = glm(Sexual ~ HQ, family = poisson(), data = HQFG)
summary(HQSL.glm)
abline(HQSL.glm)
#residual deviance is greater than degrees of freedom (923.64 vs 206) so it's over dispersed
sexuallichen.glm = glm(Sexual ~ HQ, data = HQFG, family = quasipoisson())
summary(sexuallichen.glm)

lm(Sexual ~ HQ, HQFG)
plot(Sexual ~ HQ, HQFG)
abline( )
