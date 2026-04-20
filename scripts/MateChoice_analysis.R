library(DHARMa)
library(mclogit)
library(dplyr)
library(tidyr)
library(janitor)
library(readxl)
library(survival)
library(car)


mate_data <- (read_excel("data/Mate choice FINAL.xlsx", sheet=2)
                  |> clean_names()
                  |> dplyr::select(day,arena,lineage_pair,s_male_mated)
)

#1 indicated the selection male mated, 0 indicates the control male mated

summary(mate_data)

table(mate_data$s_male_mated) 
table(mate_data$s_male_mated, mate_data$s_male_mated, useNA = "ifany")

addmargins(
  prop.table(
    table(mate_data$lineage_pair, mate_data$s_male_mated), 2), 1)

addmargins(
  prop.table(
    table(mate_data$day, mate_data$s_male_mated), 2), 1)


fit.clr <- clogit(s_male_mated ~ day + lineage_pair,
                  data = mate_data)

summary(fit.clr)


model_mixed <- mclogit(cbind(s_male_mated, suburb) ~ day, 
                       random = ~1|lineage_pair, 
                       data = mate_data)




mate <- glmer(data=mate_data, success ~ treatment + day + (1|lineage_pair), family = "binomial")

check_model(mate)
summary(mate)


