#circadian rhythm
#linear mixed models (“lme4 ” package)
#Fixed effects: treatment, sex
#Random effects: monitor (1 vs 2) lineage, day

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

circ <- readRDS("../data/clean_circ.rds")