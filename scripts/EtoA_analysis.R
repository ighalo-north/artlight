library(readxl)
library(DHARMa)
library(lme4)
library(tidyr)
library(dplyr) 
library(janitor) 
library(ggplot2)
library(performance)

etoa_data <- readRDS("data/etoa_data_clean.rds")

head(etoa_data)
summary(etoa_data)

etoa_data <- transform(etoa_data,
                    trt_lin = interaction(treatment, lineage),
                    trt_lin_vial = interaction(treatment, lineage, vial))

etoa_survival <- glmer(eclosed ~ treatment + (1|trt_lin) + (1|trt_lin_vial), family = "binomial", data=etoa_data)

summary(etoa_survival)
check_model(etoa_survival) 

with(etoa_data, 
     table(trt_lin_vial, eclosed)) 
#the influential observations identified by check_model are the instances where only one or two flies from that vial did not eclose.
#this makes sense, and I don't think is too concerning.


duration_data <- filter(etoa_data, eclosed == 1)
head(duration_data)
summary(duration_data)
duration_data$sex <- as.factor(as.character(duration_data$sex))

duration_data <- transform(duration_data,
                    trt_lin = interaction(treatment, lineage),
                    trt_lin_vial = interaction(treatment, lineage, vial))

etoa_duration <- glmer(time ~ treatment*sex + (1|trt_lin) + (1|trt_lin_vial), data=duration_data, family=Gamma(link="log"))

summary(etoa_duration)
check_model(etoa_duration) 
#Data points 284 and 283 are influential observations because they eclosed unusually late.
#Should still be included in analysis in my opinion.


