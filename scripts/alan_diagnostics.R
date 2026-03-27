library(ggplot2)
library(magrittr)
library(tidyverse)
library(emmeans)


options(contrasts=c("contr.sum", "contr.poly"))

alan <- readRDS("../data/clean_alan_gen25.rds")

#small model----
linalan_null <- lm(Lightscore ~ 1, data = alan) #null hypothesis
linalanbasic <- lm(Lightscore~TrtLin + Generation, data=alan)

#draw diagnostic
performance::check_model(linalanbasic)
performance::check_model(linalan_null)

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

linalan <- (Lightscore !)


#compare the fit of each model to my data
drop1(linalan, test="F")

#draw diagnostic
performance::check_model(linalan)

anova(linalanbasic, linalan_null, linalan)
