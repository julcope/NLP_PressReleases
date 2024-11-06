# Header ---- 
## use 4 trailing dashes to create outline
##### Purpose: to find the sentences examples for the confusion matrix
##### Author: Julia Cope
##### Creation Date: 05/07/23
##### Project: Capturing Climate Claims 
##### Inputs: 12_topics_in_climate_df

##### Output: 

# Libraries ----
library(readr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(dplyr)


# READ in data ---- 
in_climate_df <- read_csv("12_topics_climate_df.csv")
in_all_sentences <- read_csv("02_PR_sentences.csv")
X04D_FULL_labeled_sampledata <- read_csv("04D_FULL_labeled_sampledata.csv")

merged_df <- X04D_FULL_labeled_sampledata %>% 
                                  merge(in_climate_df[, c("index", "company", "Year", "X1")], by = c("index", "company", "Year"),
                                        all.x = TRUE)
