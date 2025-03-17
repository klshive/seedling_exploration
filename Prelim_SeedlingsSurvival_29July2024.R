library(ggplot2)
library(lme4)
library(tidyr)
library(dplyr)
library(emmeans)
library(pbkrtest)
# library(lmerTest)
library(ggeffects)
library(glmmTMB)
library(DHARMa)
library(stringr)
library(boot)
library(tidyverse)

sdlg = read.csv("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\SEGI planting\\Data\\plots_survival_factors_2023.csv")
head(sdlg)
nrow(sdlg)

sdlg$GIS_Aspect.radians <- (sdlg$aspect * pi / 180)
sdlg$Northness = cos(sdlg$GIS_Aspect.radians)

#all seedlings 
remo = sdlg %>%
  filter(grove == "REMO")
rm <- glmmTMB(ALL ~ Northness, data = remo, family = binomial, weights = Total_ALL)
summary(rm)
simulationOutput <- simulateResiduals(fittedModel = rm, plot = T)

all <- glmmTMB(ALL ~ Northness + (1|grove), data = sdlg, family = binomial, weights = Total_ALL)
summary(all)
simulationOutput <- simulateResiduals(fittedModel = all, plot = T)

#pines
rm.pine <- glmmTMB(PINE ~ Northness, data = subset(remo, remo$PINE!= -1), family = binomial, weights = Total_PINE)
summary(rm.pine)
simulationOutput <- simulateResiduals(fittedModel = rm.pine, plot = T)

rmbo.pine <- glmmTMB(PINE ~ Northness + (1|grove), data = subset(sdlg, sdlg$PINE!= -1), family = binomial, weights = Total_PINE)
summary(rmbo.pine)
simulationOutput <- simulateResiduals(fittedModel = rmbo.pine, plot = T)

#pines
rm.segi <- glmmTMB(SEGI ~ Northness, data = subset(remo, remo$SEGI!= -1), family = binomial, weights = Total_SEGI)
summary(rm.segi)
simulationOutput <- simulateResiduals(fittedModel = rm.segi, plot = T)

rmbo.segi <- glmmTMB(SEGI ~ Northness + (1|grove), data = subset(sdlg, sdlg$SEGI!= -1), family = binomial, weights = Total_SEGI)
summary(rmbo.segi)
simulationOutput <- simulateResiduals(fittedModel = rmbo.segi, plot = T)



boca = sdlg %>%
  filter(grove == "BOCA")

ggplot(remo, aes(Northness, ALL)) + geom_point()
ggplot(boca, aes(Northness, ALL)) + geom_point()
ggplot(sdlg, aes(Northness, ALL, col = grove)) + geom_point()
ggplot(sdlg, aes(aspect, ALL, col = grove)) + geom_point()

ggplot(remo, aes(slope_perc, ALL)) + geom_point()
ggplot(boca, aes(slope_perc, ALL)) + geom_point()
ggplot(sdlg, aes(slope_perc, ALL, col = grove)) + geom_point()


##########################
## seedlings as the sample unit
sdlg.all = read.csv("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\SEGI planting\\Data\\REMO_BOCA_Survival_by_Seedling.csv")
head(sdlg.all)
nrow(sdlg.all)

c = left_join(sdlg.all,sdlg, by="PlotID")
head(c)
nrow(c)
# View(c)


c$GIS_Aspect.radians <- (c$aspect * pi / 180)
c$Northness = cos(c$GIS_Aspect.radians)
c$LD = ifelse(c$LiveStatus.LD=="L",1,0)

all <- glmmTMB(LD ~ slope_perc + (1|grove/PlotID), data = c, family = binomial)
summary(all)
simulationOutput <- simulateResiduals(fittedModel = all, plot = T)

ggplot(c, aes(LiveStatus.LD, Northness)) + geom_boxplot()
ggplot(boca, aes(Northness, ALL)) + geom_point()
ggplot(sdlg, aes(Northness, ALL, col = grove)) + geom_point()
ggplot(sdlg, aes(aspect, ALL, col = grove)) + geom_point()
