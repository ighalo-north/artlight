# Journal

This is where we will record anything we do that involves looking at patterns in the data (i.e., anything but data cleaning) and record plans for how we will proceed with our analysis

### Plans

-   if the diagnostic plots look okay, we will look at the summary table

-   we will decide what measurement scales each of our response variables are on (interval, ordinal, ratio, etc)

### Statistical plans 

-   artificial selection behavioural data:
    -   response variable: lightscore
-   egg to adult survival and developmental duration:
    -   response variable: day/time of eclosion,
-   longevity:
    -   4 males 4 females in each of 20 vials per lineage
        -   note: only 1 in 4 single sex flies died at all in any vial except S1 6, where 2 females died
    -   Vial ID will be treated as a random effect in the statistical model
-   mate choice:
    -   response variable:
-   circadian rhythm:

<!-- -->

-   (for example, “if the diagnostic plots look OK, we will look at the summary table and use the clarity of the response to phosphorus as our main outcome”. (See Simmons, Nelson, and Simonsohn (2011).)

Longevity - Vial ID random effect in statistical model

Statistics Mating Success Statistics: compare proportion of flies mated using GLM with a binomial distribution; treatment (fixed), set (random); response variables: choice and mating latency Statistics. Compare the proportion of males mated using GLMM with a binomial distribution, with treatment as a fixed factor and lineage pair as random factor (Scott et al., 2022). Explore mating latency as well. response vars: choice and mating latency Caveats. Note that day for the selection lineages is from 9 PM to 9 AM, and that day for the wild flies is from 10 AM to midnight. I have decided not to alter this mismatch. If we find a difference in mating success, we may do a follow up test during the selection lineages’ morning.

Circadian Rhythm Trait of interest: baseline evolved activity between populations. Each comparison of activity between populations will be run through linear mixed models (“lme4 ” package) Fixed effects: treatment, sex Random effects: monitor (1 vs 2) lineage, day To account for the effects of circadian rhythm on activity, either impose a circadian periodicity (sin(pihour/12) + cos(pihour/12)) or fit a natural smooth cubic spline with 5 knots (ns(hour, 5)) for hours. Start by looking at 1 h totals (counts per hour)

Trait of interest: baseline evolved activity between populations. Each comparison of activity between populations will be run through linear mixed models (“lme4 ” package) Fixed effects: treatment, sex Random effects: monitor (1 vs 2) lineage, day To account for the effects of circadian rhythm on activity, either impose a circadian periodicity (sin(pihour/12) + cos(pihour/12)) or fit a natural smooth cubic spline with 5 knots (ns(hour, 5)) for hours. Start by looking at 1 h totals (counts per hour)

Egg-to-Adult Survival

Developmental Duration

Longevity
