# Header ---- 
## use 4 trailing dashes to create outline
##### Purpose: to plot the histograms all in one row for the paper. code copied from other scripts like 01 and 03
##### Author: Julia Cope
##### Creation Date: 05/06/23
##### Project: Capturing Climate Claims 
##### Inputs: 12_topics_in_climate_df

##### Output: 

# Libraries ----
library(readr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(directlabels)

#Set Working direction ----
#wd_base <- "~/OneDrive/Desktop/NLP"
#setwd("..")
#setwd("Desktop")
#setwd("NLP")
setwd("03_Outputs")
getwd()


# READ in data ---- 
in_climate_df <- read_csv("12_topics_climate_df.csv")
in_all_sentences <- read_csv("02_PR_sentences.csv")

#CLEAN data ----
in_climate_df$Date <- as.Date(in_climate_df$Date,format = "%m/%d/%Y")
in_climate_df$monthyear <- format(in_climate_df$Date, "%m/%Y") # format date to mm/yyyy

in_all_sentences$Date <- as.Date(in_all_sentences$Date,format = "%m/%d/%Y")
in_all_sentences$monthyear <- format(in_all_sentences$Date, "%m/%Y") # format date to mm/yyyy
in_all_sentences <- na.omit(in_all_sentences)

# create a vector of ordered monthyears
ordered_monthyears <- unique(in_climate_df$monthyear)
ordered_monthyears <- ordered_monthyears[order(as.Date(paste0(ordered_monthyears, "-01"), "%Y-%m-%d"))]

# convert monthyear to ordered factor
in_climate_df$monthyear <- factor(in_climate_df$monthyear, levels = ordered_monthyears)
in_all_sentences$monthyear <- factor(in_all_sentences$monthyear, levels = ordered_monthyears)


## make the company graphs ----
## group by index, company, and monthyear ----
cleaned_climate_sent_monthly <- in_climate_df %>%
  group_by(index, company, monthyear) %>%
  summarise(climate_sent_count = n(),
  topics = paste(topic, collapse = ", "))


cleaned_All_sent_monthly <- in_all_sentences %>%
  group_by(index, company, monthyear) %>%
  summarise(in_all_sentences_count = n())


## merge climate sentence count into full RPR df ----
cleaned_All_sent_monthly <- merge(cleaned_All_sent_monthly, 
                          cleaned_climate_sent_monthly[, c("index", "company", "monthyear", "climate_sent_count")], 
                   by = c("index", "company", "monthyear"), all.x = TRUE)

cleaned_All_sent_monthly$climate_sent_count[is.na(cleaned_All_sent_monthly$climate_sent_count)] <- 0

## calculate avg sentence prpportion
cleaned_All_sent_monthly$sent_per_article <- cleaned_All_sent_monthly$climate_sent_count / cleaned_All_sent_monthly$in_all_sentences_count


hist(cleaned_All_sent_monthly[cleaned_All_sent_monthly$sent_per_article != 0, ]$sent_per_article)


ggplot(cleaned_All_sent_monthly[cleaned_All_sent_monthly$sent_per_article != 0, ], aes(y = sent_per_article)) +
  geom_boxplot() +
  ylab("proportion of climate sentences")


## has climate sentence ----
cleaned_All_sent_monthly$has_clima_sent <- cleaned_All_sent_monthly$climate_sent_count != 0 


 #PLOTS----
cleaned_All_sent_monthly$Year <- format(as.Date(cleaned_All_sent_monthly$monthyear,format = "%M/%Y"), format = "%Y")

## line plot of yearly PR ----
yearly_PR_total <- cleaned_All_sent_monthly %>% group_by(Year) %>%
  summarize(`total press releases` = n())

g_totalPR <- ggplot(yearly_PR_total, aes(x = Year, y = `total press releases`)) +
  geom_line(aes(group = 1), size = 1, color = "darkblue") +
  geom_point(shape = 16, size = 2.5, color = "darkblue") +
  xlab("Year") + ylab("total press releases") +
  ggtitle("Total Press Releases per Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g_totalPR



### bar plot of clima sent ----

has_clima_sent_year <- aggregate(has_clima_sent ~ Year, data = cleaned_All_sent_monthly, sum)

g_bar_CCsent<-ggplot(has_clima_sent_year, aes(x = Year, y = has_clima_sent)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  xlab("Month Year") + ylab("Count") +
  ggtitle("Press Releases with 1 or more climate sentences per Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"))
g_bar_CCsent


## line plot of yearly CC PR ----

has_clima_sent_year <- has_clima_sent_year[order(has_clima_sent_year$Year),] # Sort data by year
has_clima_sent_year <- has_clima_sent_year%>% merge(yearly_PR_total, by = c("Year"))


g_count_ccPR <- ggplot(has_clima_sent_year, aes(x = Year, y = has_clima_sent)) +
  geom_line(aes(group = 1), size = 1, color = "#0047AB") +
  geom_point(shape = 16, size = 2.5, color = "#0047AB") +
  xlab("Year") + ylab("Total Number of Press Releases") +
  ggtitle("Number of Press Releases with >1 Climate Change Sentence") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g_count_ccPR


ggsave("13_g_count_ccPR.png",
       plot = g_count_ccPR, device = "png", dpi = 300)

## line plot of prop cc PR ----
has_clima_sent_year$prop <- has_clima_sent_year$has_clima_sent / has_clima_sent_year$`total press releases`
g_prop_ccPR <- ggplot(has_clima_sent_year, aes(x = Year, y = prop)) +
  geom_line(aes(group = 1), size = 1, color = "#85C124") +
  geom_point(shape = 16, size = 2.5, color = "#85C124") +
  xlab("Year") + ylab("Proporion of Press Releases") +
  ggtitle("Proportion of Press releases with >1 Climate Change Sentence") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g_prop_ccPR

ggsave("13_g_prop_ccPR.png",
       plot = g_prop_ccPR, device = "png", dpi = 300)


## plot with comp ----
PR_tot_with_co <- cleaned_All_sent_monthly %>% group_by(Year, company) %>%
  summarize(`total press releases` = n())

g_comps <- ggplot(PR_tot_with_co, aes(x = as.numeric(Year), y = `total press releases`,color = company)) +
  geom_line(aes(group = company), size = 1) +
  geom_point(shape = 16) +
  xlab("Year") + ylab("total press releases") +
  ggtitle("Total Press Releases per Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g_comps <- g_comps + scale_color_manual(values = c("Exxon" = "#F01523", 
                                       'Chevron' = "#0054A4", 
                                       'Marathon' = "#0081C6",
                                       'Phillips' = "#000000", 
                                       'Valero' = "#FFB900"))
g_comps <- g_comps+ theme(legend.position = "none")

g_comps <- g_comps + geom_dl(aes(label = company, x = as.numeric(Year)), method ="last.qp")
g_comps <- g_comps+ scale_x_continuous(breaks = 2000:2022,
                                       limits=c(2000, 2023),
                                       expand = c(0, 1)) +
                    scale_y_continuous(breaks = c(500, 1000, 1500, 2000, 2500, 3000, 3500))
g_comps

ggsave("13_yearly_total_sent_perco.png",
       plot = last_plot(), device = "png", dpi = 300)


# NUMBERS get numbers ----
NUM_count_per_topic <- in_climate_df %>% group_by( topic) %>% 
  summarize(`total press releases` = n()/63615)

PLOT_topic_per_year <- in_climate_df %>% group_by(Year, topic) %>% 
  summarize(`total press releases` = n())

# Create a named factor of unique topic labels
topics <- factor(unique(PLOT_topic_per_year$topic), levels = rev(unique(PLOT_topic_per_year$topic)))

strings <- c("0 Energy Solns", "1 New Tech", "2 Company Attributes", 
             "3 Risk Factors", "4 Reducing Emissions", "5 Market Growth",
             "6 Corp Responsibility","7 Renewable Diesel", "8 Future Dev")
### create a new column based on the mapping of numbers to strings----
PLOT_topic_per_year$Topics <- factor(PLOT_topic_per_year$topic, levels = 0:8, labels = strings)

topics_labels <- factor(unique(PLOT_topic_per_year$Topics), levels = rev(levels(PLOT_topic_per_year$Topics)))

### create new column for rate of topic use per year... ----
PR_year_total <- aggregate(PLOT_topic_per_year$`total press releases`, by=list(PLOT_topic_per_year$Year), sum)
PLOT_topic_per_year <- PLOT_topic_per_year %>% merge(PR_year_total, by.x = 'Year', by.y = 'Group.1')


PLOT_topic_per_year$per_year <- PLOT_topic_per_year$`total press releases` / PLOT_topic_per_year$x

# Create the heatmap ----
g_heat <- ggplot(PLOT_topic_per_year, aes(x = Year, y = Topics, fill = per_year)) +
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "#00671a") +
  labs(title = "Proportion of topical sentences per year", x = "Year", y = "")
  

g_heat <- g_heat + theme(axis.text.x = element_text(angle = 45, hjust = 1),
      panel.background = element_rect(fill = "white")) +  labs(fill = "")

g_heat

ggsave("13_heatmap_topics2.png",
       plot = g_heat, device = "png",dpi = 300)
