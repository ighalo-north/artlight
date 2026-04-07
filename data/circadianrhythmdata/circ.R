#https/rethomics.github.iodamr.html
#https/cran.r-project.orgwebpackagesdamrdamr.pdf
#documentation for damr package with examples <3 <3 <3
#bout analysis seems irrelevant but httpsrethomics.github.iosleepr.html#bout-analysis

#DAM: Drosophila Activity Monitor
#i measured the activity of each of my 8 replicate lineages using 2 monitors at a time
'
We have 2 independent monitors, each connected to their own 
  laptop running the DAM software. We tested pairs of replicates 
    simultaneously.

We will only analyze 24 hours of data for each trial (9pm to 9pm) 
  the 13 hours beforehand allows the flies to acclimate to the environment.
'

library(damr) #main pkg
library(sleepr) #identifies when an animal is asleep or not (tied to behavr pkg)
library(lubridate) #fix date format for damr
library(ggetho) #graphing activity
library(readr)
library(tidyverse)
library(dplyr)
library(stringr)
library(data.table)
library(MASS)
library(car)

metadata <- fread("metadata.csv")
metadata

files <- list.files(pattern = "Monitor.*\\.txt$")

check_gaps <- function(file) {
  df <- read.table(file, sep = "\t", header = FALSE, fill = TRUE, stringsAsFactors = FALSE)
  
  # combine date + time
  times <- as.POSIXct(
    paste(df[[2]], df[[3]]),
    format = "%d %b %y %H:%M:%S",
    tz = "UTC"
  )
  
  # compute diffs
  dt <- as.numeric(diff(times))
  
  # indices where gap > 1 hr
  idx <- which(dt > 3600)
  
  if (length(idx) > 0) {
    cat("\nFile:", file, "\n")
    for (i in idx) {
      cat("Gap:", dt[i], "seconds\n")
      print(df[i:(i+1), ])
      cat("----\n")
    }
  }
  
  return(length(idx) > 0)
}
problem_files <- files[sapply(files, check_gaps)]

df <- read.table("Monitor1.txt", sep="\t", header=FALSE, fill=TRUE, stringsAsFactors=FALSE)
times <- as.POSIXct(paste(df[[2]], df[[3]]), format="%d %b %y %H:%M:%S")

# First timestamp in file
head(times, 1)
# Compare with metadata
metadata$start_datetime[1]  # Should be the same day and hour


#reformat start and end time into the format damr wants (it's soooo picky)
metadata$start_datetime <- format(
  ymd_hm(metadata$start_datetime),
  "%Y-%m-%d %H:%M:%S"
)
metadata$stop_datetime <- format(
  ymd_hm(metadata$stop_datetime),
  "%Y-%m-%d %H:%M:%S"
)

Sys.setlocale("LC_TIME", "C")  # ensure English month names

# Function to fix hours in a file
fix_hours <- function(file) {
  # Read file as text lines
  lines <- readLines(file)
  
  # Replace single-digit hour at start of time with leading zero
  # Matches " H:MM" after the date part
  lines_fixed <- str_replace(lines, "(\\d{1,2} [A-Za-z]{3} \\d{2} )([0-9]):", "\\10\\2:")
  
  # Overwrite the file
  writeLines(lines_fixed, file)
}
# Apply to all files
lapply(files, fix_hours)
# Load metadata CSV
metadata <- read_csv("metadata_alltimes_fixed.csv", show_col_types = FALSE)
'
# Fix single-digit hour by padding with zero
metadata <- metadata %>%
  mutate(
    start_datetime = str_replace(start_datetime, "(\\d{4}-\\d{2}-\\d{2} )([0-9]):", "\\10\\2:"),
    stop_datetime  = str_replace(stop_datetime, "(\\d{4}-\\d{2}-\\d{2} )([0-9]):", "\\10\\2:")
  )

# Save back to CSV
write_csv(metadata, "metadata_alltimes_fixed.csv")
files <- list.files(pattern = "^Monitor.*\\.txt$")
'
problem_report <- list()

for (f in files) {
  lines <- readLines(f)
  split_lines <- strsplit(lines, "\\s+")
  cols_per_row <- sapply(split_lines, length)
  
  truncated_rows <- which(cols_per_row < 4)
  
  # only rows with >=3 columns for timestamps
  valid_lines <- which(cols_per_row >= 3)
  timestamps <- sapply(split_lines[valid_lines], function(x) paste(x[2], x[3]))
  
  times <- as.POSIXct(timestamps, format = "%d %b %y %H:%M:%S", tz = "America/New_York")
  
  na_time_rows <- valid_lines[is.na(times)]
  
  # intervals
  valid_time_idx <- which(!is.na(times))
  dt <- diff(times[valid_time_idx])
  non60_idx <- which(as.numeric(dt, units="secs") != 60)
  non60_rows <- sort(unique(c(valid_time_idx[non60_idx], valid_time_idx[non60_idx + 1])))
  non60_rows <- valid_lines[non60_rows]
  
  problem_report[[f]] <- list(
    truncated_rows = truncated_rows,
    na_time_rows = na_time_rows,
    non60_rows = non60_rows
  )
  
  cat("File:", f, "\n")
  cat("  Truncated rows:", truncated_rows, "\n")
  cat("  Timestamp parse failures:", na_time_rows, "\n")
  cat("  Non-60s interval rows:", non60_rows, "\n\n")
}

#link da dam data
metadata <- link_dam_metadata(metadata, result_dir = getwd())
metadata #view da dam data

#create a column for TrtLin format
metadata$TrtLin <- factor(paste(metadata$treatment, metadata$lineage, sep = ))

#treat all of these factors as factors
metadata <- metadata |> 
  mutate(across(c("lineage", "treatment", "TrtLin", "sex"), as.factor))
#metadata$region_id <- as.factor(metadata$region_id) #scared to mess with the built in variables

#identifies when an animal is asleep or not (tied to behavr pkg)
#time_window_length default is 300 (unit is seconds, 5 minutes); #of seconds to be used by the motion classifier. This corresponds to the sampling period of the output data.
#time_window_length = #reset to
#min_time_immobile Minimal duration (in seconds) of a sleep bout. Immobility = to this value are considered as sleep.
#min_time_immobile = #reset to

dtallflies <- load_dam(metadata) #load only good data

#good practice do not include (or even look at) vials whose fly died or escaped
#but i might also just include them until death
dt <- load_dam(metadata[status=="OK"], FUN = sleepr::sleep_dam_annotation) #load only good data
dtnotnone <- load_dam(metadata[status!="none"], FUN = sleepr::sleep_dam_annotation) #load only good data
summary(dt)

dt_curated <- curate_dead_animals(dt) #remove data of dead animals after they died (done by a movement threshold or smthg)
dt[, moving := activity > 0] #new col for moving (true false) if they are crossing da beams
dt_curated[, uniqueid := .I]
dt_curated[, sleep_fraction := mean(asleep), by = id]


dt_curated <- dt_curated %>%
  mutate(monitor = as.integer(str_extract(id, "(?<=Monitor)\\d+")))

#DO NOT CHANGE THE ABOVE----- it's working finally!!!

#compute overall average fraction of time spent moving
#the average time spent moving per 1000 (rounded)
mean_mov_dt <- dt[, .(mean_moving = round(mean(moving), 1000)), by=id]
#join curent meta and the summary table
new_meta <- dt[mean_mov_dt, meta=T]
#set new metadata
setmeta(dt, new_meta)
head(dt[meta=T])

#mean activity
activity_dt <- dt[,
                  .(mean_acti = mean(activity),
                    max_acti = max(activity)
                  ),
                  by='id']
activity_dt


#day and time
dt[, datetime := sub("\\|.*", "", id)]
dt[, day := as.integer(as.Date(datetime) - min(as.Date(datetime)))]
dt[, day2 := as.integer(factor(date))]
summary(dt$day)
summary(dt$day2)




#light phase info
dt[, phase := ifelse((t %% (24*3600)) < (12*3600), "L", "D")]

summary_dt <- 
  rejoin(dt[,
            .(
              #this is where the computation happens
              sleep_fraction = mean(asleep),
              sleep_fraction_all = mean(asleep),
              sleep_fraction_l = mean(asleep[phase == "L"]),
              sleep_fraction_d = mean(asleep[phase == "D"]),
              move_fraction = mean(moving),
              move_fraction_all = mean(moving),
              move_fraction_l = mean(moving[phase == "L"]),
              move_fraction_d = mean(moving[phase == "D"])
            ),
            ,by=id])
summary_dt

summary_dt <- summary_dt |>
  mutate(monitor = as.integer(str_extract(id, "(?<=Monitor)\\d+")))
summary_dt <- summary_dt |>
  mutate(monitor=as.factor(monitor))

saveRDS(summary_dt, "../../data/clean_circ.rds")










#example of anova
#If we are interested in the effect of sex AND genotype,
#as well as their interaction, we can model our response
#variable with a formula sleep_fraction_all ~ sex  genotype
model <- aov(sleep_fraction_all ~ sex*TrtLin, data = summary_dt)
summary(model)
model <- aov(move_fraction_all ~ sex*TrtLin, data = summary_dt)
summary(model)


#graph sleep----
sleep_dt <- dt_curated[, .(sleep_fraction = mean(asleep)), by = id]
sleep_dt <- dt_curated[sleep_dt]

ggplot(sleep_dt, aes(x = treatment, y = sleep_fraction)) +
  stat_summary(
    fun = mean, 
    geom = "point", 
    color = "red", 
    size = 3
  ) +
  stat_summary(
    fun.data = mean_cl_normal,  # mean ± 95% CI
    geom = "errorbar", 
    width = 0.2,
    color = "red"
  ) +
  theme_minimal() +
  labs(
    x = "Treatment",
    y = "Sleep Fraction",
    title = "Sleep Fraction by Treatment"
  )
ggplot(sleep_dt, aes(x = treatment, y = sleep_fraction)) +
  # raw data points, jittered slightly for visibility
  geom_jitter(width = 0.1, alpha = 0.3, color = "gray40", size = 1.5) +
  
  # mean point
  stat_summary(fun = mean, geom = "point", color = "#D55E00", size = 4) +
  
  # 95% CI error bars
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.15, color = "#D55E00", size = 1) +
  
  theme_minimal(base_size = 14) +
  labs(
    x = "Treatment",
    y = "Sleep Fraction",
    title = "Sleep Fraction by Treatment"
  ) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )


#graph other stuff----
#shows one replicate at a time
ggetho(dt[xmv(TrtLin) == 'C3'], aes(z=activity)) +
  stat_tile_etho() + #showsthe response var in the (colour) z axis
  stat_ld_annotations()

#ggetho default 30 minutes

plot1 <- ggetho(dt, aes(x=t, y=id, z=moving)) + stat_tile_etho()
plot1 #each pixel is a 30 minute mean

bar_plot <- ggetho(dt[xmv(TrtLin) == 'C1'], aes(x=t, z=moving)) + stat_bar_tile_etho()
bar_plot

#todo: add light/dark cycle in background, rename id
#todo: metadata_alltimes to trim, and then set zeitburger time UGH

#show all, pretty graph
allplot <- ggetho(dt, aes(x=t, y=TrtLin, z=moving)) + stat_bar_tile_etho()
allplot


'
Sometimes, we also want to aggregate individuals per group. 
For instance, males average vs females average. 
This can be done by changing the y axis. 
Previously, we used id, which made one row per individual. 
Instead, if we use a grouping variable like sex, 
  we will plot one row per value of sex (i.e. two rows, one for males, 
    one for females). In other words, we replace id by sex on the y axis
'
pl <- ggetho(dt, aes(x=t, y=TrtLin, z=moving)) + stat_bar_tile_etho()
pl #shows our z variable by the height of the tiles

#PRETTY
#multiple groups (population) on same graph
pl <- ggetho(dt, aes(x=t, y=moving, colour = treatment)) + stat_pop_etho()
pl
pl <- ggetho(dt, aes(x=t, y=moving)) + stat_pop_etho() +
  facet_grid(sex ~ .)
pl
pl <- ggetho(dt, aes(x=t, y=moving)) + stat_pop_etho() +
  facet_grid(treatment ~ .)
pl


pl <- ggetho(dt, aes(x=t, y=moving, colour = sex)) +
  stat_pop_etho() +
  facet_grid(treatment ~ .)
pl


'
When behaviours are periodic, 
we sometimes want to average our variable at the same time over 
consecutive days. In ggetho, we call that time wrapping. 
It can be done simply with the time_wrap argument. 
It will work the same for population or tile plots
'
pl <- ggetho(dt, aes(x=t, y=moving), time_wrap = hours(24)) + stat_pop_etho()
pl

'
If you are interested in events that happen between the 
  end and the start 
  of the wrapping period (e.g. at ZT24). 
You may want to wrap time with an “offset”. 
That is a phase shift. For instance, if we want to 
  have ZT06 in the middle of our graph, we use an 
  offset of +6h
 ' 

pl <- ggetho(dt, aes(x=t, y=moving), 
            time_wrap = hours(24),
            time_offset = hours(9)) + stat_pop_etho()
pl

'
In circadian experiments, we often like to add annotations 
  (black and white boxes) to show Dark and Light phases.
  We have another layer for that

To put the annotation in the background, 
  we can invert the order of the layers, 
  set the height of the annotation to 1 (100%) and 
  add some transparency (alpha = 0.3). 
We also remove the outline of the boxes
'
pl <- ggetho(dt, aes(x = t, y = moving, group = treatment, colour = treatment), time_wrap = 24*3600) + 
  stat_pop_etho(aes(fill = treatment), alpha = .5) + 
  stat_ld_annotations() + 
  scale_color_manual(values = c("#C69214", "#6E6E6E"))+
  scale_fill_manual(values = c("#C69214", "#6E6E6E"))+
  coord_cartesian(ylim=c(0, 1))+
  labs(y = "proportion of time spent moving")
pl

pl <- ggetho(dt, aes(x=t, y=moving, group = treatment, colour = treatment)) +
  stat_ld_annotations(height=1, alpha=0.3, outline = NA, period = 1260) +
  stat_pop_etho() +
  scale_color_manual(values = c("#C23E00", "#0A3A8A"))+
  labs(y = "proportion of time spent moving")
pl




ggplot(summary_dt, aes(x=sex, y=sleep_fraction, fill=sex)) + 
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(alpha=.5) +
  facet_grid(TrtLin ~ .) +
  scale_y_continuous(name= "Fraction of time sleeping")

ggplot(summary_dt, aes(x=interaction(sex, treatment), y=sleep_fraction, fill=sex)) + 
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(alpha=.5) +
  scale_y_continuous(name= "Fraction of time sleeping")+
  scale_fill_manual(values = c("lightpink", "lightblue"))

ggplot(summary_dt, aes(x=interaction(sex, treatment), y=move_fraction, fill=sex)) + 
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(alpha=.5) +
  scale_y_continuous(name= "Fraction of time moving")+
  scale_fill_manual(values = c("lightpink", "lightblue"))

ggplot(summary_dt, aes(x=interaction(sex, treatment), y=move_fraction_d, fill=sex)) + 
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(alpha=.5) +
  scale_y_continuous(name= "Fraction of time moving at night")+
  scale_fill_manual(values = c("lightpink", "lightblue"))

ggplot(summary_dt, aes(x=interaction(sex, treatment), y=sleep_fraction_l, fill=sex)) + 
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(alpha=.5) +
  scale_y_continuous(name= "Fraction of time sleeping at day")+
  scale_fill_manual(values = c("lightpink", "lightblue"))







