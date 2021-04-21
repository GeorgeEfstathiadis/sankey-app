library(dplyr)

# dataset stored in data with each unique subject having one row stored in PatientID column
# in columns node1, node2 and node3 we have the locations of the subjects at those 3 timepoints

## store column names for timepoints here
timepoints <- c('node1', 'node2', 'node3')
for (i in 1:(length(timepoints)-1)){
	if (i == 1){
		new_data <- data.frame(USUBJID = data$PatientID, PATHNO = i, NODE_S = data[,timepoints[i]],
                    NODE_E = data[,timepoints[i+1]])
	} else {
		new_data2 <- data.frame(USUBJID = data$PatientID, PATHNO = i, NODE_S = data[,timepoints[i]],
                    NODE_E = data[,timepoints[i+1]])
		new_data <- rbind(new_data, new_data2)
	}
}
## merge back with original data to access filters in the dashboard
new_data <- merge(new_data, data, by.x='USUBJID', by.y='PatientID')
write.csv(new_data,'data.csv')
