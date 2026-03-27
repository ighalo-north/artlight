# Journal

This is where we will record anything we do that involves looking at patterns in the data (i.e., anything but data cleaning) and record plans for how we will proceed with our analysis

### Plans

-   for each dataset, if the diagnostic plots look okay, we will look at the summary tables

-   we will decide what measurement scales each of our response variables are on (interval, ordinal, ratio, etc)

### Statistical Plans

**Artificial Selection Behavioural Data**

-   In part based on Scott, Dworkin, and Dukas (2022; Animal Behaviour) We will analyze generations 1-25 of the artificial selection experiment in a single mixed-effects general linear model, fitted using the lmer function from the R package lme4. We will assess the significance of the fixed effects used Anova from the car pkg

-   We will analyze the effect of relaxed selection by fitting a model of lightscores from generation 25, which was the last generation with artificial selection, and generation 35. The model was fitted and fixed effects tested in the same form as described above for the Generation 1–25 model

-   response variable: lightscore

-   **Fixed Effects**

    -   Generation
    -   Sex
    -   Treatment
    -   Interested in all interactions between the three variables.
    -   Time of day

-   **Random Effects**

    -   Day (Generation\|Day)

    -   Maze Position

    -   Lineage -\> (Treatment\|Lineage)

    -   Maze ID

    -   Light side (nested in Maze ID) -\> (Maze\|Light Side) ?? Do we need to include this? Should be a fixed effect because it has two levels?

    -   Model would look something like: lightscore \~ Generation*Sex*Treatment + Time of Day + (Generation\|Day) + (1\|Maze_position) + (Treatment\|lineage) + (1\|Maze ID)

**Egg-to-Adult Survival and Developmental Duration**

-   Planning:

    -   response variable: did fly eclose (binary) + day/time of eclosion

    -   vial nested within lineage, lineage nested within treatment

-   Will fit using a cox mixed effects regression, which should nicely incorporate both response variables

-   Response Variables: eclosed + time

-   Fixed effects: treatment, sex

-   Random effects: lineage (nested within treatment), vial (nested within lineage) - Might look like: (lineage\|treatment/vial)

**Longevity**

-   4 males and 4 females in each of 20 vials per lineage
    -   note: only 1 in 4 single sex flies died at all in any vial except S1 6, where 2 females died
-   Vial ID will be treated as a random effect in the statistical model
-   response variable: did fly survive the week (binary) + day/time of death
-   Will fit a cox mixed effects regression, since it is frequently used for survival data similar to ours
-   Response Variables: FlyLifespan & Status
-   Fixed Effects: Treatment, Sex (+ interaction)
-   Random Effects: Lineage (nested within treatment, Vial (nested within lineage))
-   Like this? (Treatment\|Lineage/Vial)

**Mate Choice**

-   data layout

    -   condition: in each vial is 1 Control male and 1 Selection male

    -   selection male mated: 1 - selection male mated, 0 - control male mated

    -   80 individuals per lineage, this sheet only records vials where mating occurs

        -   S male mated: 1 - selection male mated, 0 - control male mated

    -   if mating did not happen w either male, row is gone (all lines have 80 or less individual rows)

    -   should be 640 rows once data are generated

-   response variables: choice

    -   we are ignoring mating latency

-   this_male_colour is a nuisance variable

-   compare proportion of males mated using GLMM with a binomial distribution; with treatment as a fixed factor and lineage pair as random factor (Scott et al., 2022).

-   Caveats. Note that day for the selection lineages is from 9 PM to 9 AM, and that day for the wild flies is from 10 AM to midnight. We have decided not to alter this mismatch. If we find a difference in mating success, we may do a follow up test during the selection lineages’ morning.

<!-- -->

-   Response Variable: Mate success
-   Fixed Effects: treatment
-   Random effects: lineage pair, day/arena

**Circadian Rhythm**

-   response variable: activity (baseline evolved activity between populations)
-   Each comparison of activity between populations will be run through linear mixed models (“lme4 ” package) Fixed effects: treatment, sex Random effects: monitor (1 vs 2) lineage, day
-   To account for the effects of circadian rhythm on activity, either impose a circadian periodicity (sin(pihour/12) + cos(pihour/12)) or fit a natural smooth cubic spline with 5 knots (ns(hour, 5)) for hours. we will start by looking at 1 h totals (counts per hour)
