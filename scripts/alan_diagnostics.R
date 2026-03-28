library(ggplot2)
library(magrittr)
library(tidyverse)
library(emmeans)
library(DHARMa)
library(emmeans)
library(MASS)
library(car)
library(lme4)


options(contrasts=c("contr.sum", "contr.poly"))

alan <- readRDS("../data/clean_alan_gen25.rds")

#small model----
linalan_null <- lm(Lightscore ~ 1, data = alan) #null hypothesis
linalanbasic <- lm(Lightscore~TrtLin + Generation, data=alan)

#draw diagnostic for simple linear models
performance::check_model(linalanbasic)
performance::check_model(linalan_null)

#compare the two simple linear models
anova(linalanbasic, linalan_null)


#BIG MODEL (includes hypothesis about Females vs Males----
#make linear model for hypothesis----
#add vars one by one
linalan <- lm(Lightscore~TrtLin + Generation, data = alan)

linalan <- update(linalan, . ~ . +Sex)

linalan <- update(linalan, . ~ . +day)

linalan <- update(linalan, . ~ . +time_of_day)

linalan <- update(linalan, .~. +Maze)

linalan <- update(linalan, . ~ . +Maze_Order)

linalan <- update(linalan, .~. +blind)

linalan <- update(linalan, .~. + Light_Side)

linalan <- update(linalan, .~. + flies_in)

#vs a linear mixed model (the proper way to analyze this data)
lmeralan <- lmer(Lightscore ~ Generation*Sex*Treatment + time_of_day + (Generation|day) + (1|maze_position) + (Treatment|Lineage) + (1|Maze),
               data = alan)

#compare the fit of each model to my data
drop1(linalan, test="F")
drop1(lmeralan, test="none") #test=Chisq? or user?


#draw diagnostic for linear model
performance::check_model(linalan)

#draw diagnostic for linear mixed model
#https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
testDispersion(lmeralan)
simulationOutput <- simulateResiduals(fittedModel = lmeralan, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)

#high collinearity between TrtLin and day, so we removed TrtLin from the model
linalan <- update(linalan,~. -TrtLin)
performance::check_model(linalan)

anova(linalanbasic, linalan_null, linalan)
anova(lmeralan)
