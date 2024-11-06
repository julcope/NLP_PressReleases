# Header ---- 
## use 4 trailing dashes to create outline
##### Purpose: to combine unlabeled data with transformer classifier labels, output filtered climate sentences
##### Author: Julia Cope
##### Creation Date: 05/03/23
##### Project: Capturing Climate Claims 
##### Inputs: 04C_unlabeled_data.csv
##### Inputs: 07_first37thou_labels.csv
##### Inputs: 07_second37thou_labels.csv
##### Inputs: 07_first37thou_labels.csv

##### Output: 

# Libraries ----
library(readr)

#Set Working direction ----
wd_base <- "/OneDrive/Desktop/NLP"
#setwd("03_Outputs")
#setwd(wd_base)
getwd()
# Read in data ---- 
X04C_unlabeled_data <- read_csv("04C_unlabeled_data.csv")
X07_07_full_labels <- read_csv("07_full_labels.csv",    col_names = FALSE)
X04D_FULL_labeled_sampledata <- read_csv("04D_FULL_labeled_sampledata.csv")

# Process the unlabeled data ----
## append first, second, third, fourth, ----
nrow(X07_07_full_labels)
labeled_df <- cbind(X04C_unlabeled_data, X07_07_full_labels)

## filter climate sentences ---- 
labeled_df <-  labeled_df %>% filter(X1 == "1")


# Process the hand labeled data ----
## filter climate sentences ----
filtered_handlabeled <-  X04D_FULL_labeled_sampledata %>% filter(ClimateLabel == "1")
filtered_handlabeled <- filtered_handlabeled %>% select(-'climate_score', -'likely')
filtered_handlabeled$X1 <-  filtered_handlabeled$ClimateLabel
filtered_handlabeled <- filtered_handlabeled %>% select(-'ClimateLabel')

# Combine hand labeled and transformer labeled ---- 
Climate_dataset <- rbind(filtered_handlabeled,labeled_df)

# export to csv ----
write.csv(Climate_dataset, "11_climate_dataset.csv")







