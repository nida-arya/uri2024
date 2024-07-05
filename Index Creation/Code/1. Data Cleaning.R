# Load libraries
library(readr)
library(dplyr)

# Set working directory
setwd("C:/Users/nida_/Downloads/uri research")

# Clear environment
rm(list = ls())

# Read CSV, skipping metadata rows
eis <- read_csv("Data/Input/EIS Canada Occupation Minor Group.csv", skip = 16, n_max = 818)

# Add column with geography
eis <- eis %>%
  mutate(Canada = "Canada")

# Rename columns
colnames(eis) <- c("occupation", "employment", "median.income", "mean.income", "geography")

# Correct specific occupation entry (remove footnote)
eis$occupation <- gsub("00018 Seniors managers - public and private sector 15", "00018 Seniors managers - public and private sector", eis$occupation)

# Save the combined data frame to a CSV file
write.csv(eis, file = "C:/Users/nida_/Downloads/uri research/Data/Output/eis_merge.csv", row.names = FALSE)

# Extract noc code from occupation column
eis$noc_code <- sub("^([0-9]+).*", "\\1", eis$occupation)

# Separate code from name
eis$noc_code <- gsub("[^0-9]", "", eis$occupation)

#only keep 5 digit occupations (NOC 2016 4 digit == 5 digit in NOC 2021)
eis$nchar <- nchar(eis$noc_code) 
eis <- subset(eis, nchar == "5")

write.csv(eis, file = "Data/Output/eis_5digit.csv")


eis_onetnoc_remote <- read.csv("Data/Input/eis_onetnoc_remote.csv")

#Replace Canada[1] with Canada
eis_onetnoc_remote$geography <- gsub("Canada \\[1\\]", "Canada", eis_onetnoc_remote$geography)

# Only take entries where Canada is in 
eis_onetnoc_remote <- eis_onetnoc_remote[eis_onetnoc_remote$geography == "Canada", ]

eis_filtered <- read.csv("Data/Output/eis_5digit.csv")




