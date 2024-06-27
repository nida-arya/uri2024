setwd("C:/Users/nida_/Downloads/remotework_replication")
rm(list = ls())

files_directory <- "C:/Users/nida_/Downloads/remotework_replication/Data/Input/EIS"

#Getting a list of CSV files in the directory
files <- list.files(path = files_directory, pattern = "\\.csv$", full.names = TRUE)

data_list <- list()

#Loop and append each file to the list
for (file in files) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  data_list <- c(data_list, list(df))
}

#Combine all data frames into a single data frame
combined_data <- do.call(rbind, data_list)

#Rename columns
colnames(combined_data) <- c("occupation", "employment", "median.income", "mean.income", "geography")


#save file
write.csv(combined_data, file = "eis_merge.csv")