noc_mapping <- read.csv("Data/Input/correpondence_table/noc2016v1_3-noc2021v1_0-eng.csv")

# Merge 2016 data with NOC mapping
mapped2016 <- merge(eis_onetnoc_remote, noc_mapping, by.x = "noc_code", by.y = "NOC.2016.V1.3.Code", all.x = TRUE)

mapped2016 <- subset(mapped2016, select = c("NOC.2021.V1.0.Code", "remote_work"))

# Merge 2021 data with NOC mapping
mapped2021 <- merge(eis_filtered, mapped2016, by.x = "noc_code", by.y = "NOC.2021.V1.0.Code", all.x = TRUE)

write.csv(mapped2021, file = "Data/Output/mapped.csv")

# MANUAL MERGING

manual_remote_work <- read.csv("Data/Input/gallacher_hossain/eis_manual_remote.csv")

# Replace Canada[1] with Canada
manual_remote_work$geography <- gsub("Canada \\[1\\]", "Canada", manual_remote_work$geography)

# Filter rows where geography is in the Canadian regions
manual_remote_work <- manual_remote_work[manual_remote_work$geography == "Canada", ]

# Merge 2016 data with NOC mapping
mapped2016_manual <- merge(manual_remote_work, noc_mapping, by.x = "noc_code", by.y = "NOC.2016.V1.3.Code", all.x = TRUE)

# Rename column remote_work to remote_work_manual
library(dplyr)
mapped2016_manual <- mapped2016_manual %>% 
  rename(remote_work_manual = remote_work)

mapped2016_manual <- subset(mapped2016_manual, select = c("NOC.2021.V1.0.Code", "remote_work_manual"))

# Merge 2021 data with NOC mapping
mapped2021 <- merge(mapped2021, mapped2016_manual, by.x = "noc_code", by.y = "NOC.2021.V1.0.Code", all.x = TRUE)

# Create manual_scores and onetnoc_scores datasets
manual_scores <- subset(mapped2021, !is.na(remote_work_manual))
manual_scores <- manual_scores[, !(names(manual_scores) %in% c("X", "nchar", "remote_work"))]
manual_scores <- manual_scores %>% 
  distinct(noc_code, .keep_all = TRUE)
write.csv(manual_scores, file = "Data/Output/manual_scores.csv")

onetnoc_scores <- subset(mapped2021, !is.na(remote_work))
onetnoc_scores <- onetnoc_scores[, !(names(onetnoc_scores) %in% c("X", "nchar", "remote_work_manual"))]
onetnoc_scores <- onetnoc_scores %>% 
  distinct(noc_code, .keep_all = TRUE)
write.csv(onetnoc_scores, file = "Data/Output/onetnoc_scores.csv")
