#clean raw data
library(readxl)
library(tidyverse)
library(skimr)
library(ggplot2)

alan <- read_excel("../data/raw_alan_gen25.xlsx")
sapply(alan,class)

(alan #examine the structure of the data and check for problems
  |> summary()
  |> print())

skim(alan) #more examine structure, check for missing vals

#throw out the redundant dates (redundant with their _num equivalents)
alan <- subset(alan, select = -c(start_time, end_time, Time_Elapsed))

#how many generations
generations_total <- (tail(na.omit(alan$Generation, n = 1)))[1]

#fill blind column (BEFORE making it a factor)
alan$blind <- ifelse(alan$Generation <= 10, "no", alan$blind)

#use tidyverse to change some char variables to factors
(alan <- alan
  |> mutate(across(c(Maze, Lineagae, Sex, Treatment, Maze_Order, blind, time_of_day, Light_Side), as.factor)))
#order of the levels doesn't matter in my experiment, no reason to reorder

#lineage should be nested within treatment for all analyses
alan$TrtLin <- interaction(alan$Treatment, alan$Lineage, drop = TRUE, sep = "")

vial_number1 <- match(c("1"), names(alan))
vial_number2 <- match(c("16"), names(alan))
vials <- c(vial_number1:vial_number2) #indexes of cols with vial numbers (count data about the flies' position)

#find anything that's not numeric in the numbers sections
should_be_numeric <- c("Generation", "day", "Lineage", 1:16, "Lightscore", "total_Flies", "start_time_num", "end_time_num", "time_elapsed_num", "flies_in", "prop_out") ## list of columns (indices or names)

find_bad_nums <- function(x){
  x_num <- suppressWarnings(as.numeric(x))
  which(!is.na(x) & is.na(x_num))
  return(x_num)
}

find_bad_nums(alan$x)
lapply(alan, find_bad_nums)

#force all vals that should be numeric to be numeric
(alan <- alan 
  |> mutate(across(all_of(should_be_numeric), as.numeric))
)

#manipulating data so the graphs are prettier
alan_sum <- (alan 
             |> summarise(across(c(Lightscore, prop_out),
                                 .fns = list(
                                   mean = ~ mean(.,na.rm=TRUE),
                                   se   = ~ sd(., na.rm = TRUE) / sqrt(sum(!is.na(.x))))),
                          .by = c(Generation, TrtLin, Sex)
             )
)

skim(alan_sum) #summary to make sure it worked - yay

#check that no lineage has >4 data points per generation
check <- alan_sum |> count(TrtLin, Generation)
stopifnot(all(check$n <= 4))


#fill light side column based on maze column
alan$Light_Side <- ifelse(alan$Maze %in% c("A", "C"), "L", "R")

#use tidyverse to change remaining char variables to factors
(alan <- alan
  |> mutate(across(c(Light_Side, day), as.factor)))


#plot as sanity check output
gg0 <-  
  ggplot(alan_sum, aes(x=Generation, y=Lightscore_mean,
                       colour=TrtLin, group=TrtLin)) +
  geom_point()+
  geom_smooth(method="lm", formula = 'y~x', se=TRUE)+
  scale_x_continuous(limits = c(1, generations_total), breaks = 1:24)+
  scale_y_continuous(limits = c(1, 16), breaks = 1:16)+
  labs(title="selection lineages' lightscore over generation", y="lightscore",
       x="generation")+
  theme_bw()

print(gg0)
gg0 + filter(alan_sum, stringr::str_detect(TrtLin, "^C"))
gg0 + filter(alan_sum, stringr::str_detect(TrtLin, "^S"))

#one facet per treatment:
alan_sum2 <- mutate(alan_sum, grp = substr(TrtLin, 1, 1))
gg0 + alan_sum2 + facet_wrap(~grp,  nrow = 1)

#Use the saveRDS function in R to save a clean (or clean-ish) version of your data
saveRDS(alan, "../data/clean_alan_gen25.rds")





