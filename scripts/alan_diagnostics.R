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

alan <- alan |> 
  mutate(Generation_factor = as.factor(Generation))
#vs a linear mixed model (the proper way to analyze this data)----
lmeralan <- lmer(Lightscore ~ Generation*Sex*Treatment + time_of_day + (Generation|day) + (1|maze_position) + (Treatment|Lineage) + (1|Maze),
                 data = alan)
drop1(lmeralan, test="Chisq") #test=Chisq? or none? --> same output actually so nvm
  #test=user is pointless atp

#okay so what if we use a gamma distribution with glmm----
gg0 <- ggplot(alan,aes(Generation_factor,Lightscore))+geom_point()

gg1 <- gg0 + geom_smooth(method="glm",colour="red",
                         formula=y~x,
                         method.args=list(family=Gamma(link="log")))
gg1 <- glm(Lightscore ~ Generation_factor*Sex*Treatment + time_of_day + (Generation_factor|day) + (1|maze_position) + (Treatment|Lineage) + (1|Maze), data = alan, family=quasipoisson(link="log"))
summary(gg1)

mymodel <- glmer(Lightscore ~ Generation*Sex*Treatment + time_of_day + (1 | Generation_factor/day) + (1|maze_position) + (1 | Treatment/Lineage) + (1|Maze) + (1|Maze/Light_Side), data = alan, family=Gamma(link="log"))

'
1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  Model failed to converge with max|grad| = 0.0303779 (tol = 0.002, component 1)
  See ?lme4::convergence and ?lme4::troubleshooting.
2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  Model is nearly unidentifiable: very large eigenvalue
 - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
 - Rescale variables?
'
#from BB on stackoverflow
  #the basic problem is that you have multiple 
  #observations in your data set of x per m, but they all have the same response value, so your x 
  #random effect is confounded with everything else

#removed 1|maze/light_side
mymodel <- glmer(Lightscore ~ Generation*Sex*Treatment + time_of_day + (1 | Generation_factor/day) + (1|maze_position) + (1 | Treatment/Lineage) + (1|Maze), data = alan, family=Gamma(link="log"))
mod_matrix <- model.matrix(mymodel)
colnames(mod_matrix)

#remove (1|Maze)
mymodel <- glmer(Lightscore ~ Generation*Sex*Treatment + time_of_day + (1 | Generation_factor/day) + (1|maze_position) + (1 | Treatment/Lineage) + (1|Maze/Light_Side), data = alan, family=Gamma(link="log"))
#failed to converge


plot(gg1) #diagnostics 1 by 1
performance::check_model(gg1)

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

