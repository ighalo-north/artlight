#clean raw data
library(readxl)
library(tidyverse)
library(skimr)
library(ggplot2)
library(forcats)

alan <- read_excel("data/raw_alan_gen25.xlsx")
sapply(alan,class)

(alan #examine the structure of the data and check for problems
  |> summary()
  |> print())

skim(alan) #more examine structure, check for missing vals

#throw out the redundant dates (redundant with their _num equivalents)
alan <- subset(alan, select = -c(start_time, end_time, Time_Elapsed))

#how many generations
generations_total <- (tail(na.omit(alan$Generation, n = 1)))[1]

#factorize alan and break it into three levels
alan$Generation_factor <- as.factor(alan$Generation)

alan$Generation_split <- fct_collapse(alan$Generation_factor,
                                      Q1 = c(1, 2, 3, 4, 5),
                                      Q2 = c(6, 7, 8, 9, 10),
                                      Q3 = c(11, 12, 13, 14, 15),
                                      Q4 = c(16, 17, 18, 19, 20),
                                      Q5 = c(21, 22, 23, 24, 25))


#fill blind column (BEFORE makGeneration_factor#fill blind column (BEFORE making it a factor)
alan$blind <- ifelse(alan$Generation <= 10, "no", alan$blind)

#use tidyverse to change some char variables to factors
(alan <- alan
  |> mutate(across(c(Maze, Lineage, Sex, Treatment, Maze_Order, blind, time_of_day, Light_Side), as.factor)))
#order of the levels doesn't matter in my experiment, no reason to reorder

#lineage should be nested within treatment for all analyses
alan$TrtLin <- interaction(alan$Treatment, alan$Lineage, drop = TRUE, sep = "")

vial_number1 <- match(c("1"), names(alan))
vial_number2 <- match(c("16"), names(alan))
vials <- c(vial_number1:vial_number2) #indexes of cols with vial numbers (count data about the flies' position)

#find anything that's not numeric in the numbers sections
should_be_numeric <- c("Generation", "Lineage", "day", "Lineage", 1:16, "Lightscore", "total_Flies", "start_time_num", "end_time_num", "time_elapsed_num", "flies_in", "prop_out") ## list of columns (indices or names)

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
                          .by = c(Generation, TrtLin, Sex, Treatment, Lineage)
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

#change Maze_Order to a position for each maze
alan <- alan %>%
  mutate(
    maze_position = case_when(
      Maze == "A" & Maze_Order == "ABCD" ~ 1,
      Maze == "B" & Maze_Order == "ABCD" ~ 2,
      Maze == "C" & Maze_Order == "ABCD" ~ 3,
      Maze == "D" & Maze_Order == "ABCD" ~ 4,
      Maze == "A" & Maze_Order == "CDAB" ~ 3,
      Maze == "B" & Maze_Order == "CDAB" ~ 4,
      Maze == "C" & Maze_Order == "CDAB" ~ 1,
      Maze == "D" & Maze_Order == "CDAB" ~ 2,
      Maze == "A" & Maze_Order == "DBCA" ~ 4,
      Maze == "B" & Maze_Order == "DBCA" ~ 2,
      Maze == "C" & Maze_Order == "DBCA" ~ 3,
      Maze == "D" & Maze_Order == "DBCA" ~ 1,
      TRUE ~ NA
    )
  )
alan <- alan |> 
  mutate(maze_position = as.factor(maze_position))

alan$TrtLin <- factor(alan$TrtLin, levels = c("S1", "S2", "S3", "S4", "C1", "C2", "C3", "C4"))

#create a repeating trial id (1 trial is 1 maze, 4 trials run consecutively (most of the time))
alan <- alan |>
  group_by(Date) |>
  mutate(rep_trial_id = as.integer(factor(Maze))) |>
  ungroup()


colorsA <- c("red", "purple", "orange", "darkred")
colorsB <- c("lightblue", "darkblue", "blue", "lightgreen")

blue_base <- c("#08306B", "#4292C6")     # deep → medium blue
orange_base <- c("#7F2704", "#E6550D")   # dark → light orange
blue_gradient <- colorRampPalette(blue_base)(4)
orange_gradient <- colorRampPalette(orange_base)(4)
mypalette <- c(blue_gradient, orange_gradient)

gg0 <-  
  ggplot(alan_sum, aes(x=Generation, y=Lightscore_mean,
                       colour=TrtLin, group=TrtLin)) +
  geom_point()+
  geom_smooth(method="lm", formula = 'y~x', se=TRUE)+
  scale_x_continuous(limits = c(1, generations_total), breaks = 1:generations_total)+
  scale_y_continuous(limits = c(1, 16), breaks = 1:16)+
  labs(title="Lineages' lightscore over generation", y="lightscore",
       x="generation")+
  theme_bw()

print(gg0)
gg0 + filter(alan_sum, stringr::str_detect(TrtLin, "^C")) + scale_color_manual(values=orange_gradient)
gg0 + filter(alan_sum, stringr::str_detect(TrtLin, "^S")) + scale_color_manual(values=blue_gradient)

#one facet per treatment
gg0 + alan_sum + facet_wrap(~Treatment,  nrow = 1) + scale_color_manual(values = mypalette, labels = c("S1", "S2", "S3", "S4", "C1", "C2", "C3", "C4")) 

#Use the saveRDS function in R to save a clean (or clean-ish) version of your data
saveRDS(alan, "data/clean_alan_gen25.rds")





