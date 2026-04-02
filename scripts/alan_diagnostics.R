library(ggplot2)
library(magrittr)
library(tidyverse)
library(emmeans)
library(DHARMa)
library(emmeans)
library(MASS)
library(car)
library(lme4)
library(lmerTest)

options(contrasts=c("contr.sum", "contr.poly"))

#working directory MUST be source file location - there's a way to automate this but later
alan <- readRDS("../data/clean_alan_gen25.rds")

#small model----
linalan_null <- lm(Lightscore ~ 1, data = alan) #null hypothesis
linalanbasic <- lm(Lightscore~TrtLin + Generation, data=alan)

#draw diagnostic for simple linear models
performance::check_model(linalanbasic)
performance::check_model(linalan_null)

#compare the two simple linear models
anova(linalanbasic, linalan_null)


#BIG MODEL (includes hypothesis about Females vs Males----
#make linear model for hypothesis----
#add vars one by one
linalan <- lm(Lightscore~TrtLin + Generation, data = alan)

linalan <- update(linalan, . ~ . +Sex)

linalan <- update(linalan, . ~ . +day)

linalan <- update(linalan, . ~ . +time_of_day)

linalan <- update(linalan, .~. +Maze)

linalan <- update(linalan, . ~ . +Maze_Order)

linalan <- update(linalan, .~. +blind)

linalan <- update(linalan, .~. + Light_Side)

linalan <- update(linalan, .~. + flies_in)
#compare the fit of each model to my data
drop1(linalan, test="F")
#draw diagnostic for linear model
performance::check_model(linalan)


#vs a linear mixed model (the proper way to analyze this data)----
lmeralan <- lmer(Lightscore ~ Generation*Sex*Treatment + time_of_day + (Generation|day) + (1|maze_position) + (Treatment|Lineage) + (1|Maze),
                 data = alan)
drop1(lmeralan, test="Chisq") #test=Chisq? or none? --> same output actually so nvm
  #test=user is pointless atp

#draw diagnostics for linear mixed model
testDispersion(lmeralan)
plot(simulateResiduals(fittedModel = lmeralan, plot = F))

#trying to fix ks test results
simulationOutput <- simulateResiduals(lmeralan)
plot(simulationOutput)
for (var in c("Sex", "Generation", "Treatment", "time_of_day", "day", "maze_position", "Lineage", "Maze")){
  plotResiduals(simulationOutput, form = alan[[var]])
  
} #yikes

testDispersion(lmeralan)


#high collinearity between TrtLin and day, so we removed TrtLin from the model
linalan <- update(linalan,~. -TrtLin)
performance::check_model(linalan)

anova(linalanbasic, linalan_null, linalan)
anova(lmeralan)


#plot distributions across vials by lineage----
#data are in wide format, shift to long
long_count <- pivot_longer(
  alan,
  cols = `1`:`16`,
  names_to = "vial",
  values_to = "flies"
)

long_count$vial <- as.numeric(long_count$vial)
#this type of thing, one row per observation,
  #might be better for analysis? like if we want to switch
  #lightscore to a cat var with 5 vials per group to break up the vial choices


plotgendist <- function(generation_num){
  templongdf_subsetfungen <- filter(long_count, Generation == generation_num)
  
  temp_plot <- ggplot(templongdf_subsetfungen, aes(x=vial, y=flies, colour = TrtLin, group = TrtLin))+
    geom_line(alpha = 0.4) +   #faint lines for replicates
    stat_summary(fun = mean, geom = "line", aes(group = TrtLin, colour = TrtLin), size = 1.2) + #mean line
    scale_x_continuous(limits = c(1, 16), breaks = 1:16)+
    scale_y_continuous(limits = c(0, 60))+
    labs(x = "Vial", y = "Fly Count", title = paste0("Fly Distribution by Lineage, Gen ", generation_num)) +
    theme_minimal()
  
  rm(templongdf_subsetfungen)
  print(temp_plot)
  print("done")
}

#plot graphs for each generation, truly just for fun
for (i in 1:25){
  plotgendist(i)
}

