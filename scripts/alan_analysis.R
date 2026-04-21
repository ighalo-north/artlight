library(ggplot2)
library(magrittr)
library(tidyverse)
library(emmeans)
library(DHARMa)
library(MASS)
library(car)
library(lme4)
library(lmerTest)
library(performance)
library(glmmTMB)
library(effects)
library(DHARMa)
library(qqplotr)

options(contrasts=c("contr.sum", "contr.poly"))

alan <- readRDS("data/clean_alan_gen25.rds")

#population level model----
pop_tmb <- glmmTMB(Lightscore ~ Generation*Sex*Treatment + time_of_day + (1|Generation:Treatment:Lineage) + (1|Maze) + (1|maze_position), data = alan, family = Gamma(link="log"), se = TRUE)

plotResiduals(pop_tmb)
check_model(pop_tmb)
summary(pop_tmb)

focal.predictors = c("Sex", "Treatment", "Generation", "time_of_day")
Anova(pop_tmb, type = c("3"), test.statistic = c("Chisq"))
Effect(focal.predictors, pop_tmb)

#inferential plots for pop_tmb
#all fixed effects
alan_estimates <- emmeans(pop_tmb, ~ Sex + Treatment + Generation + time_of_day, type = "response") 
alan_estimates
plot(alan_estimates) + xlab("Estimated Lightscore")

emmip(alan_estimates, Sex ~ Treatment, CIs = TRUE) +
  ylab("Estimated Lightscore") +
  xlab("Treatment") +
  theme_bw()
#sex
alan_estimates <- emmeans(pop_tmb, ~ Sex, type = "response") 
alan_estimates
plot(alan_estimates) + xlab("Estimated Lightscore")

emmip(alan_estimates, ~ Sex, CIs = TRUE) +
  ylab("Estimated Lightscore") +
  xlab("Sex") +
  theme_bw()
#treatment
alan_estimates <- emmeans(pop_tmb, ~ Treatment, type = "response") 
alan_estimates
plot(alan_estimates) + xlab("Estimated Lightscore")

emmip(alan_estimates, ~ Treatment, CIs = TRUE) +
  ylab("Estimated Lightscore") +
  xlab("Treatment") +
  theme_bw()
#time of day
alan_estimates <- emmeans(pop_tmb, ~ time_of_day, type = "response") 
alan_estimates
plot(alan_estimates) + xlab("Estimated Lightscore")

emmip(alan_estimates, ~time_of_day, CIs = TRUE) +
  ylab("Estimated Lightscore") +
  xlab("time of day") +
  theme_bw()


#population model with generation collapsed----
'Generation$split

Q1 1, 2, 3, 4, 5 
Q2 6, 7, 8, 9, 10 
Q3 11, 12, 13, 14, 15 
Q4 16, 17, 18, 19, 20 
Q5 21, 22, 23, 24, 25'
tmbgensplit <- glmmTMB(Lightscore ~ Generation_split*Sex*Treatment + time_of_day + (1|Generation_split:Treatment:Lineage) + (1|Maze) + (1|maze_position), data = alan, family = Gamma(link="log"), se = TRUE)
plotResiduals(tmbgensplit)
focal.predictors = c("Sex", "Treatment", "Generation_split", "time_of_day")
Anova(tmbgensplit, type = c("3"), test.statistic = c("Chisq"))
Effect(focal.predictors, tmbgensplit)

alan_estimates <- emmeans(tmbgensplit, ~ Treatment | Generation_split, type = "response") 
alan_estimates
plot(alan_estimates) + xlab("Estimated Lightscore")

alan_estimates <- emmeans(tmbgensplit, ~ Generation_split | Treatment, type = "response") 
alan_estimates
plot(alan_estimates) + xlab("Estimated Lightscore")
emmip(alan_estimates, ~ Treatment |Generation_split, CIs = TRUE) +
  ylab("Estimated Lightscore") +
  xlab("Generation") +
  theme_bw()
alan_estimates <- emmeans(tmbgensplit, ~ Generation_split | Treatment*Sex, type = "response") 
alan_estimates
plot(alan_estimates) + xlab("Estimated Lightscore")


emmeans(tmbgensplit, ~ Treatment | Generation_split) |>
pairs()

emmeans(tmbgensplit, ~ Sex*Treatment | Generation_split) |>
pairs()

alan_estimates <- emmeans(tmbgensplit, ~ Sex, type = "response") 
alan_estimates
plot(alan_estimates) + xlab("Estimated Lightscore")
emmip(alan_estimates, ~ Sex, CIs = TRUE, as.table = FALSE) +
  ylab("Estimated Lightscore") +
  xlab("Generation") +
  theme_bw()


#one vial per row----
indialan <- alan |>
  mutate(trial_id = row_number()) |>
  pivot_longer(
    cols=11:26,
    names_to="vial_id",
    values_to="count"
  )
indialan$vial_id <- as.integer(indialan$vial_id)


#this dataframe is 1 row per fly
  #39,645 flies finished the maze that's crazy
indialanuncount <- indialan |>
  mutate(count = replace_na(count, 0)) |>
  uncount(count)

#individual fly level model----
#####FUTURE DIRECTIONS

flytmb <- glmmTMB(cbind(vial_id, 16-vial_id) ~ Generation_split*Sex*Treatment + time_of_day + (1|Generation_split:Treatment:Lineage) + (1|Maze) + (1|maze_position), data = indialanuncount, family = binomial(link="logit"), se = TRUE)


plot(simulateResiduals(flytmb), rank = T)

check_predictions(flytmb)
plot(check_residuals(flytmb, size_dot = 1.2))

check_collinearity(flytmb, size_dot = 1.2)
binned_residuals(flytmb, size_dot = 1.2)
check_overdispersion(flytmb)

focal.predictors = c("Sex", "Treatment", "Generation_split", "time_of_day")

Anova(flytmb, type = c("3"), test.statistic = c("Chisq"))
Effect(focal.predictors, flytmb)


#inferential plots, all fixed effects at individual level---- 
#model doesn't fit, so these plots aren't really relevant yet
alan_estimates <- emmeans(flytmb, ~ Generation_split|Treatment*Sex*time_of_day, type = "response") 
alan_estimates

emmip(alan_estimates, ~Generation_split | Treatment*Sex, CIs = TRUE, as.table = FALSE) +
  ylab("Estimated VIAL ID") +
  xlab("Treatment") +
  theme_bw()

emmip(alan_estimates, ~Treatment ~ Generation_split | time_of_day + Sex, CIs = TRUE, as.table = FALSE) +
  ylab("Estimated VIAL ID") +
  xlab("Treatment") +
  theme_bw()

emmip(alan_estimates, ~ Generation_split | Treatment, CIs = TRUE) +
  ylab("Estimated VIAL ID") +
  xlab("Generation") +
  theme_bw()














