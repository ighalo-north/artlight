# artificial light

final project for bio708: basis of animal attraction to artificial light at night

Keren Ighalo and Jack Rosenbaum

*Datasets in the `data` folder:*

Artificial Selection Data
1.  `raw_alan_gen25.xlsx`: artificial selection behavioural data 
2.  `clean_alan_gen25.rds`: artificial selection behavioural data after cleaning

Egg to Adult Survival + Developmental Duration
1.  egg to adult survival and developmental duration `etoadd_dataflies.xlsx`
2.  wide format: `EtoA_data.xslx`
3.  cleaned data ready for analysis: `etoa_data_clean.rds`

Longevity
1.  longevity raw data `Longevity_Finalold.xlsx`
2.  longevity reformatted data `Longevity_Final.xlsx`
3.  cleaned data for analysis: `clean_longev.rds`

Mate Choice
1. mate choice raw data `Mate choice FINAL.xlsx`
2. Reformatted data. Each row corresponds to a unique fly: `clean_matechoice.rds`
    - Did not end up using for final analysis

Circadian rhythm (omitted from project)
1. `circadian rhythm data` (folder w files for each monitor)
2. Cleaned data set with data from all monitors: `clean_circ.rds`

*Scripts in the `scripts` folder:*

1. `alan_cleaning.R` 
        - Reformatting of Artificial selection data `raw_alan_gen25.xlsx` to 
          produce `clean_alan_gen25.rds` ready for analysis
2. `alan_analysis.R`
        -  Modeling of data from `clean_alan_gen25.rds` with diagnostic plots, 
            inferential plots, and plots of the data. Includes a model with lightscore as 
            the response variable, but also an alternative model with individual vial as the
            response variable. The lightscore model is the model we're including for the sake
            of the project. But for the publication, we plan on further exploring the individual
            level model.
3. `circ_diagnostics.R`
        - Omitted from project
4. `EtoA_analysis.R`
        - Modeling of data from `etoa_data_clean.rds` with diagnostic plots, 
            inferential plots, and plots of the data. Separate models for the survival 
            of eggs to adulthood and how long it took them to develop. Minor filtering of
            the data is required for switching between the two analyses. Initial plans 
            were to analyse in one model, which is why both models are on one script.
5. `EtoA_cleaning.R`
        - Restructuring of egg to adults data `EtoA_data.xslx` to 
           produce `etoa_data_clean.rds` ready for analysis. Was produced with 
           the intention to model development duration and survival in one model.
6. `Longevity_analysis.R`
        - Very minor data cleaning of `Longevity_Final.xlsx`, then models that 
          data. Includes diagnostic plots, inferential plots, and plots of the data.
          Produces `clean_longev.rds` in case alternative analysis were to ever be 
          done on another script. 
7. `MateChoice_analysis.R`
        - Modeling of data straight from `Mate choice FINAL.xlsx` with diagnostic plots, 
            inferential plots, and plots of the data.
8. `MateChoice_cleaning.R`
        - Restructuring of data from `Mate choice FINAL.xlsx` for analysis. 
          Produces `clean_matechoice.rds`, but did not end up using for analysis
          since analysis plans changed.




