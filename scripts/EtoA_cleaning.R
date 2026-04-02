library(readxl)
library(tidyr)
library(dplyr)

wide_etoa <- read_excel("../data/EtoA_data.xlsx", range = 'A1:AE81') 

long_etoa <- (wide_etoa |> 
                pivot_longer(cols = 7:31, names_to = 'time', values_to = "count" ) |> 
                #Each row will represent a different time point for each vial, with the number of flies that eclosed at that time point in the 'count collumn'
                filter(count!=0) #remove vial:time points where no flies eclosed 
)

#creating a table with the total number of flies eclosed per vial
flies_eclosed <- (long_etoa |> 
                    group_by(id) |>
                    summarise(total_eclosed = sum(count, na.rm = TRUE)) #gets counts of eclosed flies per vial
                  )

flies_eclosed$not_eclosed <- 20 - ifelse(flies_eclosed$total_eclosed == "21",20,flies_eclosed$total_eclosed)
#calculates number of eggs that did not eclose. Considers instances of 21 eclosed flies, treats as 20 when calculating flies that did not eclose.

etoa_eclosed <- long_etoa |> uncount(count) |> mutate(eclosed = 1) #add row for every fly counted, then add a 1 to indicate the fly eclosed

etoa_not_ecl <- (merge(etoa_eclosed,flies_eclosed,by='id') #add info about treatment and vial to counts of uneclosed flies
                      |> distinct(id, .keep_all = TRUE) #keep one row per vial
                      |> select(-total_eclosed) #remove total eclosed column to match format of etoa_eclosed
                      |> mutate(time = 246, sex = 'NA', eclosed = 0) #change values for uneclosed flies
                      |> uncount(not_eclosed) #create row for every uneclosed fly
) 

etoa_data <- (rbind(etoa_eclosed,etoa_not_ecl) #add data for eclosed and uneclosed flies together
              |> mutate(across(where(is.character),as.factor)) 
              |> mutate(lineage = factor(lineage))
              |> mutate(time = as.numeric(time))
)

summary(etoa_data)
str(etoa_data)

saveRDS(etoa_data,"../data/etoa_data_clean.rds")

