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

ggplot1 <- ggsurvplot(fit, size = 2, censor = FALSE, legend = c(.25,.50),legend.title = "Treatment",
                      legend.labs = c("Control","Selection"), ggtheme = theme(title = element_text(size=30), axis.title.x = element_text(size=30), axis.title.y = element_text(size=30),
                                                                              legend.text = element_text(size=25), axis.text = element_text(size=18), 
                                                                              legend.title = element_text(size=25), panel.background = element_rect(fill= FALSE),
                                                                              axis.line = element_line(), plot.title = element_text(size=35, hjust = 0.5), 
                                                                              legend.key.height = unit(1.3,'cm')
                      ))$plot

ggplot1 + xlab("Age (Days)") + ylab("Proportion Alive")

longev_data <- longev_data %>%
  dplyr::select(-specific)

saveRDS(longev_data, "../data/clean_longev.rds")




