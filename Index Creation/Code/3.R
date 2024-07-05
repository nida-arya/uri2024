rm(list = ls())

manual_scores <- read_csv("Data/Output/manual_scores.csv")
onetnoc_scores <- read_csv("Data/Output/onetnoc_scores.csv")

NOC_40 <- read_csv("Data/Input/NOC_40_remote_work.csv")



colnames(NOC_40)

NOC_43 <- NOC_40 %>%
  select(noc43 = noc40, remote_work_onet, remote_work_manual)

NOC_43 <- NOC_43 %>%
  slice(1) 

NOC_43$occupation <- NA 
NOC_43$occupation[1] <- "Legislative and senior management occupations"

onetnoc_scores$two_digit <- substr(onetnoc_scores$noc_code, 1, 2)
onetnoc_scores$three_digit <- substr(onetnoc_scores$noc_code, 1, 3)


manual_scores$two_digit <- substr(manual_scores$noc_code, 1, 2)
manual_scores$three_digit <- substr(manual_scores$noc_code, 1, 3)

manual_scores$two_digit[1] <- 00
manual_scores$three_digit[1] <- 000




compute_weighted_avg <- function(noc_43, noc_vector, column, occupation_name) {
  # Filter rows where column matches the criteria in noc_vector
  filtered_manual <- manual_scores[manual_scores[[column]] %in% noc_vector, ]
  filtered_onetnoc <- onetnoc_scores[onetnoc_scores[[column]] %in% noc_vector,]
  
  weighted_avg_manual <- filtered_manual %>%
    summarise(weighted_average = weighted.mean(remote_work_manual, w = employment))
  
  weighted_avg <- filtered_onetnoc %>%
    summarise(weighted_average = weighted.mean(remote_work_manual, w = employment))
  
  df_temp <- data.frame(
    noc43 = noc_43,
    remote_work_onet = weighted_avg,
    remote_work_manual = weighted_avg_manual,
    occupation = occupation_name
  )
  print(df_temp)
  
  NOC_43 <<- rbind(NOC_43, df_temp)
  
  
}


compute_weighted_avg(2, (10), "two_digit", "telemarketer")
