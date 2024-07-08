# Load libraries
library(readr)
library(dplyr)

# Set working directory and clear environment
setwd("./Index Creation")
rm(list = ls())

## DATA CLEANING AND PREPROCESSING ##

# Read and preprocess employment income statistics (EIS) data
eis <- read_csv("Data/Input/canadian_occupation_minor_group_statistics.csv", skip = 16, n_max = 818)
eis <- eis %>%
  mutate(Canada = "Canada")

# Rename columns
colnames(eis) <- c("occupation", "employment", "median.income", "mean.income", "geography")

# Correct specific occupation entry (remove footnote)
eis$occupation <- gsub("00018 Seniors managers - public and private sector 15", "00018 Seniors managers - public and private sector", eis$occupation)

# Extract noc code from occupation column, keeping only 5 digit occupations
eis$noc_code <- sub("^([0-9]+).*", "\\1", eis$occupation)
eis$noc_code <- gsub("[^0-9]", "", eis$occupation)
eis$nchar <- nchar(eis$noc_code) 
eis <- subset(eis, nchar == "5")

write.csv(eis, file = "Data/Output/eis_merge.csv")


# Read and preprocess national-level remote work teleworkability data
eis_onetnoc_remote <- read.csv("Data/Input/Gallacher Hossain - Remote Work Dynamics/eis_onetnoc_remote.csv")
eis_onetnoc_remote$geography <- gsub("Canada \\[1\\]", "Canada", eis_onetnoc_remote$geography)
eis_onetnoc_remote <- eis_onetnoc_remote[eis_onetnoc_remote$geography == "Canada", ]

## MERGE NOC 2021 WITH NOC 2016 (GALLACHER HOSSAIN) REMOTE WORK SCORES ##

# Load correspondence table of NOC 2016 to 2021
noc_mapping <- read.csv("Data/Input/correspondence_table.csv")

# Merge GH (Gallacher Hossain) 2016 data with NOC mapping
mapped2016 <- merge(eis_onetnoc_remote, noc_mapping, by.x = "noc_code", by.y = "NOC.2016.V1.3.Code", all.x = TRUE)
mapped2016 <- mapped2016 %>%
  rename(noc2016_code = noc_code, employment2016 = employment) %>%
  select(noc2016_code, employment2016, NOC.2021.V1.0.Code, remote_work)

# Merge latest EIS statistics with mapped GH 2016 data
mapped2021 <- merge(eis, mapped2016, by.x = "noc_code", by.y = "NOC.2021.V1.0.Code")
onetnoc_scores <- subset(mapped2021, !is.na(remote_work))

# If duplicate in NOC 2021 codes, take a weighted average
onetnoc_scores <- onetnoc_scores %>%
  group_by(noc_code) %>%
  mutate(weighted_average = sum(remote_work * employment2016) / sum(employment2016)) %>%
  ungroup() %>%
  distinct(noc_code, .keep_all = TRUE) %>%
  select(noc_code, employment, median.income, mean.income, weighted_average)

onetnoc_scores <- onetnoc_scores %>% rename(remote_work = weighted_average)

write.csv(onetnoc_scores, file = "Data/Output/onetnoc_scores.csv")

# Load GH manual remote work scores
manual_remote_work <- read.csv("Data/Input/Gallacher Hossain - Remote Work Dynamics/eis_manual_remote.csv")

# Replace Canada[1] with Canada
manual_remote_work$geography <- gsub("Canada \\[1\\]", "Canada", manual_remote_work$geography)
manual_remote_work <- manual_remote_work[manual_remote_work$geography == "Canada", ]

# Merge 2016 data with NOC mapping
mapped2016_manual <- merge(manual_remote_work, noc_mapping, by.x = "noc_code", by.y = "NOC.2016.V1.3.Code", all.x = TRUE)
mapped2016_manual <- mapped2016_manual %>%
  rename(noc2016_code = noc_code, employment2016 = employment, remote_work_manual = remote_work) %>%
  select(noc2016_code, employment2016, NOC.2021.V1.0.Code, remote_work_manual)

# Merge manual data with latest EIS statistics
mapped2021_manual <- merge(eis, mapped2016_manual, by.x = "noc_code", by.y = "NOC.2021.V1.0.Code")
manual_scores <- subset(mapped2021_manual, !is.na(remote_work_manual))

# If duplicate in NOC 2021 codes, take a weighted average
manual_scores <- manual_scores %>%
  group_by(noc_code) %>%
  mutate(weighted_average = sum(remote_work_manual * employment2016) / sum(employment2016)) %>%
  ungroup() %>%
  distinct(noc_code, .keep_all = TRUE) %>%
  select(noc_code, employment, median.income, mean.income, weighted_average) %>%
  rename(remote_work_manual = weighted_average)

write.csv(manual_scores, file = "Data/Output/manual_scores.csv")
