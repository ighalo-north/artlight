library(ggplot2)
library(magrittr)
library(tidyverse)
library(emmeans)
library(DHARMa)
library(emmeans)
library(MASS)
library(car)
library(lme4)
library(lmerTest)

options(contrasts=c("contr.sum", "contr.poly"))

circ <- readRDS("../data/clean_circ.rds")


'
response variable: activity (baseline evolved activity between populations)
-   Each comparison of activity between populations will be run through linear mixed models (“lme4 ” package)
-   Fixed effects: treatment, sex
-   Random effects: monitor (1 vs 2) lineage, day
-   To account for the effects of circadian rhythm on activity, 
    either impose a circadian periodicity (sin(pihour/12) + cos(pihour/12)) 
    or fit a natural smooth cubic spline with 5 knots (ns(hour, 5)) for hours. we will start by looking at 1 h totals (counts per hour)
'
circmodel <- lmer(activity ~ Sex*Treatment + 
                   (1 | monitor) + (1 | Treatment:Lineage) + (1|day), data = circ)
circmodel2 <- llmer(mean_moving ~ treatment + (1|lineage/id), data = fly_summary)

summary(circmodel)


