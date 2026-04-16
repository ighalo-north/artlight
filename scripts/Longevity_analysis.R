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
library(performance)
library(emmeans)
library(car)

longev_data <- (read_excel("data/Longevity_Final.xlsx") 
                |> mutate(across(where(is.character),as.factor))
                |> janitor::clean_names()
)
summary(longev_data)

longev_data$status <- ifelse(longev_data$fly_lifespan == "8",0,1) #add a status column that indicates if the fly died during the experiment
longev_data$vial <- as.factor(longev_data$vial)
longev_data$lineage <- as.factor(longev_data$lineage)

head(longev_data)
str(longev_data)

longev_data <- transform(longev_data,
                    trt_lin = interaction(treatment, lineage),
                    trt_lin_vial = interaction(treatment, lineage, vial))

m1 <- coxme(Surv(fly_lifespan, event=status) ~ treatment*sex + (1|trt_lin) + (1|trt_lin_vial), data=longev_data) 

summary(m1)
check_model(m1) #does not work with cox mixed effects regression, it needs to be done manually

#Schoenfeld Residuals test - check proportional hazards assumptions
test_ph <- cox.zph(m1)
ggcoxzph(test_ph)


fit <- survfit(Surv(fly_lifespan, event=status) ~ treatment, data=longev_data) #fit for the purpose of making the figure
fitbysex <- survfit(Surv(fly_lifespan, event=status) ~ treatment+sex, data=longev_data) #fit for the purpose of making the figure

#Inferential plots
longev_estimates <- emmeans(m1, ~ sex + treatment, type = "response") 
longev_estimates
plot(longev_estimates) + xlab("Estimated Survival Response")

emmip(longev_estimates, sex ~ treatment, CIs = TRUE) +
  ylab("Estimated Survival Response") +
  xlab("Lineage") +
  theme_bw()


#Plotting the effect of treatment without dividing up sexes
treatment_fit <- survfit(Surv(fly_lifespan, event=status) ~ treatment, data=longev_data) #fit for the purpose of making the figure

treatment_surv_curve <- ggsurvplot(treatment_fit, linewidth = 1, censor = FALSE, legend = c(.25,.50),
                      legend.title = "Treatment",
                      legend.labs = c("Control","Selection"), 
                      ggtheme = theme(title = element_text(size=30), 
                                      axis.title.x = element_text(size=30), 
                                      axis.title.y = element_text(size=30),
                                      legend.text = element_text(size=25), 
                                      axis.text = element_text(size=18), 
                                      legend.title = element_text(size=25), 
                                      panel.background = element_rect(fill= FALSE),
                                      axis.line = element_line(), 
                                      plot.title = element_text(size=35, hjust = 0.5), 
                                      legend.key.height = unit(1.3,'cm')
                      ))$plot

treatment_surv_curve + xlab("Age (Days)") + ylab("Proportion Alive")


#Plotting the effect of treatment striated by sex
sex_fit <- survfit(Surv(fly_lifespan, event=status) ~ treatment + sex, data=longev_data) #fit for the purpose of making the figure

sex_surv_curve <- ggsurvplot(sex_fit, linewidth = 1, censor = FALSE, legend = c(.25,.50),
                              legend.title = "Treatment",
                              legend.labs = c("Control - F",'Control - M','Selection - F',"Selection - M"),
                              ggtheme = theme(title = element_text(size=30), 
                                  axis.title.x = element_text(size=30), 
                                  axis.title.y = element_text(size=30),
                                  legend.text = element_text(size=25), 
                                  axis.text = element_text(size=18), 
                                  legend.title = element_text(size=25), 
                                  panel.background = element_rect(fill= FALSE),
                                  axis.line = element_line(), 
                                  plot.title = element_text(size=35, hjust = 0.5), 
                                  legend.key.height = unit(1.3,'cm')
                                   ))$plot

sex_surv_curve + xlab("Age (Days)") + ylab("Proportion Alive") + ylim(0.9,1) 
#Reduced the size of the y axis for better visualization.
#However this figure may be misleading.

Anova(m1)

ggplot2 <- ggsurvplot(fitbysex, size = 1, censor = FALSE,legend.title = "",
                      legend.labs = c("ControlFemale","ControlMale", "SelectionFemale", "SelectionMale"), ggtheme = theme(title = element_text(size=30), axis.title.x = element_text(size=30), axis.title.y = element_text(size=30),
                                                                              legend.title = element_text(size=25), panel.background = element_rect(fill= FALSE),
                                                                              axis.line = element_line(), plot.title = element_text(size=35, hjust = 0.5), 
                                                                              legend.key.height = unit(1.3,'cm'),
                                                                              
                      ))$plot

ggplot2 + xlab("Age (Days)") + ylab("Proportion Alive") + coord_cartesian(ylim = c(.95, 1))


longev_data <- longev_data %>%
  dplyr::select(-specific)

saveRDS(longev_data, "/data/clean_longev.rds")




