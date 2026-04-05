library(DHARMa)
library(performance)
library(lme4)

mate_data <- readRDS('data/clean_matechoice.rds')

summary(mate_data)

mate <- glmer(data=mate_data, success ~ treatment + day + (1|lineage_pair), family = "binomial")

check_model(mate)
summary(mate)

simulationOutput <- simulateResiduals(fittedModel = mate)
testDispersion(simulationOutput)

binned_residuals(mate)

