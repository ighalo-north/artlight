library(readxl)
library(tidyr)
library(janitor)
library(dplyr)

raw_mate_data <- (read_excel("../data/Mate choice FINAL.xlsx", sheet=2)
                  |> clean_names()
                  |> select(day,arena,lineage_pair,s_male_mated)
)
 
head(raw_mate_data)

mate_choice <- data.frame( #Make new blank data frame to fill up
  treatment = character(),
  day = numeric(),
  arena = numeric(),
  lineage_pair = numeric(),
  success = numeric()
)
  

for (x in 1:2){
  for (i in 1:160) {
    
    original_row <- raw_mate_data |> filter(day == x & arena == i) #filter the data for a specific day & arena
    
    y <- ifelse(x == 1, ifelse(i < 81,1,2),ifelse(i< 81,3,4)) # determine lineage based on arena (crucial for cases with no mating)
    
    ifelse(nrow(original_row) > 0, #checks if there is a row in the original data frame with the given arena & date
            (ifelse(original_row$s_male_mated == 1, #if there is a row, did the s male mate?
                    (new_rows <- data.frame(treatment = c("Selection","Control"), day = c(x,x),arena = c(i,i),lineage_pair = c(y,y),success = c(1,0))), #If so, create a new data frame which reflects that 
                    (new_rows <- data.frame(treatment = c("Selection","Control"), day = c(x,x),arena = c(i,i),lineage_pair = c(y,y),success = c(0,1)))) #If not, create a new data frame which reflects that 
             ),
            (new_rows <- data.frame(treatment = c("Selection","Control"), day = c(x,x),arena = c(i,i),lineage_pair = c(y,y),success = c(0,0))) #If no mating occurred (the above 'original row' filter created an empty dataframe), create a new data frame which reflects that
           )  
    
    mate_choice <- bind_rows(mate_choice, new_rows)
  }
}

excluded <- c(29:40, 76:80, 160) #arenas were not used on day 1

mate_choice <- (mate_choice 
                |> mutate(across(all_of(c('treatment',"day","arena","lineage_pair")), as.factor))
                |> filter(day==2 | !(arena %in% excluded)) #remove unused arenas
                )

summary(mate_choice)   

saveRDS(mate_choice, "../data/clean_matechoice.rds")



                 