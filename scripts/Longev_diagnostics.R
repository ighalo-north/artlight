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

#working directory MUST be source file location - there's a way to automate this but later
longev <- readRDS("/data/clean_Longev.rds")

View(longev)
