library(readxl)
library(DHARMa)
library(lme4)
library(survival)
library(coxme)
library(tidyr)
library(dplyr) 
library(janitor) 
library(survminer)
library(ggplot2)

etoa_data <- readRDS("data/etoa_data_clean.rds")

head(etoa_data)
summary(etoa_data)

etoa_model <- coxme(Surv(time, event=eclosed) ~ treatment + (1|treatment/lineage/vial), data=etoa_data) 

summary(etoa_model)

performance(etoa_model) #does not work with cox mixed effects regression, it needs to be done manuallt


etoa_survival <- glmer(eclosed ~ treatment + (1|treatment:lineage:vial), family = "binomial", data=etoa_data)
summary(etoa_survival)
check_model(etoa_survival)


(1|treatment:lineage:vial)

duration_data <- filter(etoa_data, eclosed == 1)
head(duration_data)
summary(duration_data)
duration_data$sex <- as.factor(as.character(duration_data$sex))

etoa_duration <- lm(time ~ treatment*sex, data=duration_data)

etoa_duration <- glmer(time ~ treatment*sex + (1|treatment:lineage/vial), data=duration_data, family=Gamma(link="log"))



summary(etoa_duration)
check_model(etoa_duration)
