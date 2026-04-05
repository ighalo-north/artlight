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

etoa_model <- coxme(Surv(time, event=eclosed) ~ treatment + (1|treatment/lineage/vial), data=etoa_data) 

summary(etoa_model)

performance(etoa_model) #does not work with cox mixed effects regression, it needs to be done manually

#Schoenfeld Residuals test - check proportional hazards assumptions
test_ph1 <- cox.zph(etoa_model)
ggcoxzph(test_ph1)

#Check for linearity using martingale residuals - not working:( not meant for coxme I think
res_mart <- residuals(m1, type = "martingale")
ggplot(data = longev_data, aes(x = status, y = res_mart)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Martingale Residuals vs Covariate")

#Plotting deviance residuals - not working:(
res_dev <- residuals(m1, type = "deviance")
plot(res_dev, ylab = "Deviance Residuals", main = "Outlier Detection")
abline(h = 0, lty = 2)
