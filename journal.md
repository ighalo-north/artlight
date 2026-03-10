Statistical plans
Longevity
- Vial ID random effect in statistical model

Statistics
Mating Success
Statistics: compare proportion of flies mated using GLM with a binomial distribution; treatment (fixed), set (random); response variables: choice and mating latency

Circadian Rhythm
Trait of interest: baseline evolved activity between populations. 
Each comparison of activity between populations will be run through linear mixed models (“lme4 ” package) 
Fixed effects: treatment, sex
Random effects: monitor (1 vs 2) lineage, day
To account for the effects of circadian rhythm on activity, either impose a circadian periodicity (sin(pihour/12) + cos(pihour/12)) or fit a natural smooth cubic spline with 5 knots (ns(hour, 5)) for hours.
Start by looking at 1 h totals (counts per hour)

Egg-to-Adult Survival 

Developmental Duration

Longevity
