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

longev_data <- (read_excel("data/Longevity_Final.xlsx") 
                |> mutate(across(where(is.character),as.factor))
                |> janitor::clean_names()
)
summary(longev_data)
longev_data$status <- ifelse(longev_data$fly_lifespan == "8",0,1)
longev_data$vial <- as.factor(longev_data$vial)
longev_data$lineage <- as.factor(longev_data$lineage)

head(longev_data)
str(longev_data)

m1 <- coxme(Surv(fly_lifespan, event=status) ~ treatment*sex + (1|treatment/lineage/vial), data=longev_data) 

summary(m1)
check_model(m1) #does not work with cox mixed effects regression, it needs to be done manually

#Schoenfeld Residuals test - check proportional hazards assumptions
test_ph <- cox.zph(m1)
ggcoxzph(test_ph)

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




