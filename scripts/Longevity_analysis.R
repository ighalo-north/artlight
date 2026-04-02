library(readxl)
library(DHARMa)
library(car)
library(lme4)
library(survival)
library(coxme)
library(tidyr)
library(dplyr) 
library(janitor) 
library(survminer)

longev_data <- (read_excel("../data/Longevity_Final.xlsx") 
                |> mutate(across(where(is.character),as.factor))
                |> janitor::clean_names()
)
summary(longev_data)
longev_data$status <- ifelse(longev_data$fly_lifespan == "8",0,1)

#?Surv
Surv(longev_data$fly_lifespan, event=longev_data$status) # Tentative model, probably not properly done!
m1 <- coxme(Surv(fly_lifespan, event=status) ~ treatment + (1|treatment/lineage) + (1|vial), data=longev_data) 
summary(m1)
Anova(m1)

fit <- survfit(Surv(fly_lifespan, event=status) ~ treatment, data=longev_data)


ggplot1 <- ggsurvplot(fit, size = 2, censor = FALSE, legend = c(.25,.50),legend.title = "Treatment",
                      legend.labs = c("Selection","Control"), ggtheme = theme(title = element_text(size=30), axis.title.x = element_text(size=30), axis.title.y = element_text(size=30),
                                                                              legend.text = element_text(size=25), axis.text = element_text(size=18), 
                                                                              legend.title = element_text(size=25), panel.background = element_rect(fill= FALSE),
                                                                              axis.line = element_line(), plot.title = element_text(size=35, hjust = 0.5), 
                                                                              legend.key.height = unit(1.3,'cm')
                      ))$plot

ggplot1 + xlab("Age (Days)") + ylab("Proportion Alive")

longev_data <- longev_data %>%
  dplyr::select(-specific)

saveRDS(longev_data, "../data/clean_longev.rds")




