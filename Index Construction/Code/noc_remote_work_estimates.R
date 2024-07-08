# Clear working environment
rm(list = ls())

# Read in the data files
manual_scores <- read_csv("Data/Output/manual_scores.csv")
onetnoc_scores <- read_csv("Data/Output/onetnoc_scores.csv")
NOC_40 <- read_csv("Data/Input/Gallacher Hossain - Remote Work Dynamics/NOC_40_remote_work.csv")
eis_merge <- read_csv("Data/Output/eis_merge.csv")


# Prepare NOC_43 data frame
NOC_43 <- NOC_40 %>%
  select(noc40, remote_work_onet, remote_work_manual) %>%
  slice(1) %>%
  rename(noc43 = noc40) %>%
  mutate(occupation = NA)
NOC_43$occupation[1] <- "Legislative and senior management occupations"

# Add two-digit and three-digit codes to scores data frames
onetnoc_scores <- onetnoc_scores %>%
  mutate(two_digit = substr(noc_code, 1, 2),
         three_digit = substr(noc_code, 1, 3))
manual_scores <- manual_scores %>%
  mutate(two_digit = substr(noc_code, 1, 2),
         three_digit = substr(noc_code, 1, 3))

# Define the function to compute weighted averages and update NOC_43
compute_weighted_avg_noc43 <- function(noc_code, noc_vector, column, occupation_name) {
  filtered_manual <- manual_scores %>% filter(get(column) %in% noc_vector)
  filtered_onetnoc <- onetnoc_scores %>% filter(get(column) %in% noc_vector)
  
  weighted_avg_manual <- filtered_manual %>%
    summarise(weighted_average = weighted.mean(remote_work_manual, w = employment)) %>%
    pull(weighted_average)
  
  weighted_avg_onetnoc <- filtered_onetnoc %>%
    summarise(weighted_average = weighted.mean(remote_work, w = employment)) %>%
    pull(weighted_average)
  
  df_temp <- data.frame(
    noc43 = noc_code,
    remote_work_onet = weighted_avg_onetnoc,
    remote_work_manual = weighted_avg_manual,
    occupation = occupation_name
  )
  
  NOC_43 <<- rbind(NOC_43, df_temp)
}

# Read and preprocess NOC_43_Codes data
NOC_43_Codes <- read_csv("Data/Input/NOC_43_Codes.csv") %>%
  slice(2:n())

# Iterate through NOC_43_Codes, calling compute_weighted_avg function to find each NOC_43 score
for (index in 1:nrow(NOC_43_Codes)) {
  row <- NOC_43_Codes[index, ]
  noc_code <- row$noc_43
  noc_vector <- as.integer(unlist(strsplit(as.character(row$`Codes based on NOC 2021 labour variant`), ", ")))
  column <- row$`Code Digits`
  occupation_name <- row$`Occupation at main job`
  compute_weighted_avg_noc43(noc_code, noc_vector, column, occupation_name)
}

NOC_43 <- NOC_43 %>%
  rename(remote_work = remote_work_onet)

# Write the updated NOC_43 to a CSV file
write.csv(NOC_43, file = "Data/Output/NOC_43_remote_work.csv")

# Load NOC 10 codes
NOC_10_Codes <- read_csv("Data/Input/NOC_10_Codes.csv") 

# Prepare data for NOC 10 calculation
filtered_df <- eis_merge[eis_merge$noc_code %in% c("00010", "00018"), ]
total_employment_noc_00 <- sum(filtered_df$employment, na.rm = TRUE)

temp_noc <- data.frame(
  ...1 = NA,
  noc_code = 0,
  employment = total_employment_noc_00,
  median.income = NA,
  mean.income = NA,
  remote_work = NOC_43$remote_work[NOC_43$noc43 == 1],
  two_digit = 00,
  three_digit = 000)

temp_noc_manual <- data.frame(
  ...1 = NA,
  noc_code = 0,
  employment = total_employment_noc_00,
  median.income = NA,
  mean.income = NA,
  remote_work_manual = NOC_43$remote_work_manual[NOC_43$noc43 == 1],
  two_digit = 00,
  three_digit = 000)

temp_noc <- rbind(temp_noc, onetnoc_scores)

temp_noc_manual <- rbind(temp_noc_manual, manual_scores)

NOC_10 <- data.frame(
  noc10 = numeric(),
  remote_work = numeric(),
  remote_work_manual = numeric(),
  occupation = character()
)

# Define the function to compute weighted averages for NOC 10
compute_weighted_avg_noc10 <- function(noc_code, noc_vector, occupation_name) {
  filtered_manual <- temp_noc_manual %>% filter(two_digit %in% noc_vector)
  filtered_onetnoc <- temp_noc %>% filter(two_digit %in% noc_vector)
  
  weighted_avg_manual <- filtered_manual %>%
    summarise(weighted_average = weighted.mean(remote_work_manual, w = employment)) %>%
    pull(weighted_average)
  
  weighted_avg_onetnoc <- filtered_onetnoc %>%
    summarise(weighted_average = weighted.mean(remote_work, w = employment)) %>%
    pull(weighted_average)
  
  df_temp <- data.frame(
    noc10 = noc_code,
    remote_work_onet = weighted_avg_onetnoc,
    remote_work_manual = weighted_avg_manual,
    occupation = occupation_name
  )
  
  NOC_10 <<- rbind(NOC_10, df_temp)
}

# Iterate through NOC_43_Codes, calling compute_weighted_avg function to find each NOC_10 score
for (index in 1:nrow(NOC_10_Codes)) {
  row <- NOC_10_Codes[index, ]
  noc_code <- row$noc_10
  noc_vector <- as.integer(unlist(strsplit(as.character(row$`Codes based on NOC 2021 labour variant`), ", ")))
  occupation_name <- row$`Occupation at main job`
  compute_weighted_avg_noc10(noc_code, noc_vector, occupation_name)
}

NOC_10 <- NOC_10 %>%
  rename(remote_work = remote_work_onet)

# Write the updated NOC_43 to a CSV file
write.csv(NOC_10, file = "Data/Output/NOC_10_remote_work.csv")
