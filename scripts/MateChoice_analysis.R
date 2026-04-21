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
                  |> mutate(across(all_of(c("day","arena","lineage_pair")), as.factor))
)

summary(mate_data)

with(mate_data, 
     table(s_male_mated))

with(mate_data, 
     table(s_male_mated, day))

with(mate_data, 
     table(s_male_mated, lineage_pair))

mate1 <- glmer(data=mate_data, s_male_mated ~ 1 + (1|day/lineage_pair), family = "binomial")
#Is Singular, removed day

mate2 <- glmer(data=mate_data, s_male_mated ~ 1 + (1|lineage_pair), family = "binomial")
#is singular, potentially not enough variation between lineages, switched lineage pair to fixed effect

mate3 <- glm(data=mate_data, s_male_mated ~ 1 + lineage_pair, family = "binomial")

check_model(mate3)
summary(mate3)

#Inferential plots
#By lineage
mate_estimates_lineage <- emmeans(mate3, ~ lineage_pair, type = "response") 
mate_estimates_lineage
plot(mate_estimates_lineage) + xlab("Estimated Mating Rate (Selected Male)") +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red")

#Overall
mate_estimates <- emmeans(mate3, ~ 1, type = "response") 
mate_estimates
plot(mate_estimates) + xlab("Estimated Mating Rate (Selected Male)") +
 geom_vline(xintercept = 0.5, linetype = "dashed", color = "red")

test(mate_estimates)

#Visualize the emmeans with ggplot
emm_df <- as.data.frame(mate_estimates)

ggplot(emm_df, aes(x = "", y = prob)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.05) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  ylim(0, 1) +
  labs(y = "Probability of Selected Male Mating",
       x = "") +
  theme_minimal()

