library(DHARMa)
library(performance)
library(mclogit)


raw_mate_data <- (read_excel("data/Mate choice FINAL.xlsx", sheet=2)
                  |> clean_names()
                  |> dplyr::select(day,arena,lineage_pair,s_male_mated)
)

#1 indicated the selection male mated, 0 indicates the control male mated

summary(mate_data)


mate <- glmer(data=mate_data, success ~ treatment + day + (1|lineage_pair), family = "binomial")

check_model(mate)
summary(mate)


