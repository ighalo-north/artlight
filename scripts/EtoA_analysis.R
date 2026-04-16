library(readxl)
library(DHARMa)
library(lme4)
library(tidyr)
library(dplyr) 
library(janitor) 
library(ggplot2)
library(performance)
library(emmeans)
library(car)

etoa_data <- readRDS("data/etoa_data_clean.rds")

head(etoa_data)
summary(etoa_data)

etoa_data <- transform(etoa_data,
                    trt_lin = interaction(treatment, lineage),
                    trt_lin_vial = interaction(treatment, lineage, vial))

etoa_eclosion <- glmer(eclosed ~ treatment + (1|trt_lin) + (1|trt_lin_vial), family = "binomial", data=etoa_data)

summary(etoa_eclosion)
check_model(etoa_eclosion) 

with(etoa_data, 
     table(trt_lin_vial, eclosed)) 
#the influential observations identified by check_model are the instances where only one or two flies from that vial did not eclose.
#this makes sense, and I don't think is too concerning.

#Inferential plot
eclosion_estimates <- emmeans(etoa_eclosion, ~ treatment, type = "response") 
eclosion_estimates
plot(eclosion_estimates) + xlab("Estimated Eclosion Rate")


#Bar plot showing number of flies eclosed
print(ggplot(etoa_data, aes(y=eclosed, x=treatment, fill = treatment)) 
      + geom_col()
      + theme_bw()
      + ylab("Number of Flies Eclosed") 
      + xlab("Lineage")
)


#Plot of mean and bootstrapped 95% confidence intervals
print(ggplot(etoa_data, aes(x = treatment, y = eclosed)) 
      + stat_summary(fun = mean, geom = "point", size = 3) 
      + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) 
      + theme_bw() 
      + ylim(0,1)
      + ylab("Proportion of Flies Eclosed") 
      + xlab("Lineage")
)


Anova(etoa_eclosion)

##############################################
## Development Duration Analysis

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
#Data points 284 and 283 are influential observations because the eclosed unusually late.
#Should still be included in analysis in my opinion.

#Inferential plots
duration_estimates <- emmeans(etoa_duration, ~ treatment + sex, type = "response") 
duration_estimates
plot(duration_estimates) + xlab("Estimated Development Duration")

emmip(duration_estimates, sex ~ treatment, CIs = TRUE) +
  ylab("Estimated Development Duration") +
  xlab("Lineage") +
  theme_bw()


print(ggplot(duration_data, aes(y=time, x=treatment, fill = treatment)) 
      + geom_boxplot()
      + theme_bw()
      + ylab("Time of Eclosion") 
      + xlab("Lineage")
)

print(ggplot(duration_data, aes(y=time, x=treatment:sex, fill = sex)) 
      + geom_boxplot()
      + theme_bw()
      + ylab("Time of Eclosion") 
      + xlab("Lineage")
)

Anova(etoa_duration)
