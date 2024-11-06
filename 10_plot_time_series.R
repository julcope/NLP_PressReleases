# Header ---- 
## use 4 trailing dashes to create outline
##### Purpose: to plot the histograms all in one row for the paper. code copied from other scripts like 01 and 03
##### Author: Julia Cope
##### Creation Date: 04/23/23
##### Project: Capturing Climate Claims 
##### Inputs: 04_labeled_sampledata.csv

##### Output: 

# Libraries ----
library(readr)
library(lubridate)
library(ggplot2)

library(dplyr)


#Set Working direction ----
wd_base <- "/OneDrive/Desktop/NLP"
wd_scripts <- str_c(wd_base, "/03_Scripts/")
wd_clean_data <- str_c(wd_base, "/02_Outputs/01_DataOutputs/")
wd_figures <- str_c(wd_base, "/02_Outputs/02_Figures/")
setwd(wd_base)
getwd()


# Read in data ---- 
labeled_df <- read_csv("/DimeNet/home/jcope@asc.upenn.edu/PR/09_all_PR_sent_df.csv")
labeled_df$ClimateLabel <- labeled_df$likely


df <- labeled_df
df$date <- as.Date(df$Date,format = "%m/%d/%Y")
df$monthyear <- format(df$date, "%m/%Y") # format date to mm/yyyy

# create a vector of ordered monthyears
ordered_monthyears <- unique(df$monthyear)
ordered_monthyears <- ordered_monthyears[order(as.Date(paste0(ordered_monthyears, "-01"), "%Y-%m-%d"))]

# convert monthyear to ordered factor
df$monthyear <- factor(df$monthyear, levels = ordered_monthyears)


## make the comapny graphs ----
# calculate the percent of records per month that have ClimateLabel=1 per each company
df_perc <- df %>%
  group_by(company, Year) %>%
  summarize(percent = mean(ClimateLabel) * 100) %>%
  ungroup()

# create a timeseries plot
ggplot(df_perc, aes(x = Year, y = percent, color = company)) +
  geom_line() +
  labs(x = "year", y = "Percent of records with ClimateLabel=1") 


ggsave("10_timeseries.png",
       plot = last_plot(), device = "png", dpi = 300)

## also save non company labeled ----

# calculate the percent of records per month that have ClimateLabel=1 per each company
df_perc_nocomp <- df %>%
  group_by(monthyear) %>%
  summarize(percent = mean(ClimateLabel) * 100) %>% ungroup()

# convert monthyear to ordered factor


# create a timeseries plot
ggplot(df_perc_nocomp, aes(x = monthyear, y = percent, group = 1)) +
  geom_line() +
  geom_smooth(method = "loess") +
  labs(x = "Month Year", y = "Percent of records with ClimateLabel=1") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  
ggsave("10_timeseries_nocomp.png",
         plot = last_plot(), device = "png", dpi = 300)


## also all PR with years ----

# calculate the percent of records per month that have ClimateLabel=1 per each company
df_perc_nocomp_YEAR <- df %>%
  group_by(Year) %>%
  summarize(percent = mean(ClimateLabel) * 100) %>% ungroup()

# convert monthyear to ordered factor


# create a timeseries plot
ggplot(df_perc_nocomp_YEAR, aes(x = Year, y = percent, group = 1)) +
  geom_line() +
  labs(x = "Year", y = "Percent of records with ClimateLabel=1") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


ggsave("10_timeseries_nocomp_Year.png",
       plot = last_plot(), device = "png", dpi = 300)

