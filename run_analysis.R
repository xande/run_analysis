
## Script cleans sensor data from Galaxy S device for analysis.
## 1. Reads data and cleans up collumn names, sets "subject" and "activity" for each of the readings
## 2. Merges the training and test sets to create "master" data set.
## 4. Outputs that file as "data.txt".
## 5. Calculates average for key mean and std dev variables. 
## 6. Outputs calcs by activity and subject as "activity_subject_means.txt".


## The function accepts one parameter (test or train) and then processes specified set of data
## to produce resulting set with proper column names, also adding subject and activity columns
read_activity_data <- function(set) {
  
  activity_labels_file <- "UCI HAR Dataset/activity_labels.txt"
  features_file <- "UCI HAR Dataset/features.txt"
  
  subject_file <- paste("UCI HAR Dataset/",set,"/subject_",set,".txt", sep="")
  x_file <- paste("UCI HAR Dataset/",set,"/X_",set,".txt", sep="")
  y_file <- paste("UCI HAR Dataset/",set,"/y_",set,".txt", sep="")
  
  
  activity_labels <- read.table(activity_labels_file, header = FALSE)
  colnames(activity_labels) <- c("ID", "Activity")
  
  features <- read.table(features_file, header = FALSE)
  
  subject <- read.table(subject_file, header = FALSE)
  colnames(subject) <- "Subject"
  
  x_test <- read.table(x_file, header = FALSE)
  colnames(x_test) <- features[,2] # naming column names accordingly to feature names
  
  y_test <- read.table(y_file, header = FALSE)
  colnames(y_test) <- "ID"
  
  mean_std <- grep("mean\\(|std\\(", features[,2],  ignore.case = TRUE  )
  x_test_tidy <- x_test[,mean_std] # Select columns containing only Mean or Standard Deviation calculations
  
  activities <- merge(y_test, activity_labels, by.x="ID", by.y="ID")['Activity'] # Create Activities for each of the measurements
  
  data <- cbind(subject, activities, x_test_tidy) # Create final data set
  return(data)
}

data_test <- read_activity_data('test')
data_train <- read_activity_data('train')

data <-rbind(data_test, data_train)
write.table(data, "data.txt", row.name=FALSE)

aggeregated_data <- aggregate(.~Subject+Activity, data, mean)
write.table(aggeregated_data, "activity_subject_means.txt", row.name=FALSE)

