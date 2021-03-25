library(dplyr)

# dataset stored in data with each unique subject having one row per timepoint stored in PatientID column
# in column Timepoint is the timepoint and in column Node we have the locations of the subjects


data <- data[order(data$PatientID, data$Timepoint),] 
rownames(data) <- NULL
final_data <- data.frame()
for (i in 1:(max(data$Timepoint)-1)){
  new_data <- data %>% 
    filter(Timepoint %in% i:(i+1)) %>% 
    group_by(PatientID) %>%
    summarise(NODE_S = first(Node),
              NODE_E = last(Node)) %>%
    mutate(PATHNO = i)
  
  final_data <- rbind(final_data, new_data)
    
}

final_data <- final_data[order(final_data$PatientID, final_data$PATHNO),] 
# final_data$PATHNAME <- ifelse(final_data$PATHNO == 1, 'day1 - day2', 'day2 - day3') Optionally to create PATHNAME column

final_data <- merge(final_data, data, by='PatientID')
## Feel free to extract any rows you don't want
## e.g. the ones in the last pathno that do not change node
## You can handle missing values from within the app and choose to remove them if you want
# final_data <- final_data %>%
#   filter((NODE_S != NODE_E)|PATHNO != max(PATHNO))
write.csv(final_data,'data.csv')