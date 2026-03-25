library(readxl)
library(tidyr)
library(janitor)

raw_mate_data <- (read_excel("data/Mate choice FINAL.xlsx", sheet=2)
                  |> clean_names()
)

head(raw_mate_data)
