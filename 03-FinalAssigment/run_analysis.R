##run_analysis.R
##Start setting up the Working directoy.
setwd("~/Coursera/DataScienceTrack/datasciencecoursera/03-FinalAssigment")

##GOAL (1): Merges the training and the test sets to create one data set.

##Load all test data sets
subject_test <- read.table("UCI HAR Dataset\\test\\subject_test.txt")
x_test <- read.table("UCI HAR Dataset\\test\\X_test.txt")
y_test <- read.table("UCI HAR Dataset\\test\\Y_test.txt")

##Load all train data sets
subject_train <- read.table("UCI HAR Dataset\\train\\subject_train.txt")
y_train <- read.table("UCI HAR Dataset\\train\\Y_train.txt")
x_train <- read.table("UCI HAR Dataset\\train\\X_train.txt")

##Merge subject dataset by rows one above another
subject <- rbind(subject_test, subject_train)

##Merge y dataset by rows one above another
y <- rbind(y_test, y_train)

##Merge x dataset by rows one above another
x <- rbind(x_test, x_train)

##Merge all by columns 
full_dataset <- cbind(subject, y, x)
##END GOAL (1): Merges the training and the test sets to create one data set.

##GOAL (2): Extracts only the measurements on the mean and standard deviation for each measurement.
##Load columns names
features <- read.table("UCI HAR Dataset\\features.txt")
features <- as.character(features$V2)
##Add to the begging the new columns
full_features <- append(features, c("subject", "activity"), after=0)

##Set the new colnames of the dataset
colnames(full_dataset) <- full_features 
##Subsetting de full dataset with only mean and std elements
meanstd_dataset <- full_dataset[,(grepl( "mean" , names(full_dataset) ) | grepl( "std" , names(full_dataset)) 
                                  | grepl( "subject" , names(full_dataset)) | grepl("activity", names(full_dataset)))]

##END GOAL (2): Extracts only the measurements on the mean and standard deviation for each measurement.

##GOAL (3): Uses descriptive activity names to name the activities in the data set
#Load activity names
activity <- read.table("UCI HAR Dataset\\activity_labels.txt")

#Replace values for labels
meanstd_dataset$activity <- with(activity, V2[match(meanstd_dataset$activity, V1)])
##END GOAL (3): Uses descriptive activity names to name the activities in the data set

##GOAL (4): Appropriately labels the data set with descriptive variable names.
colnames(meanstd_dataset)<-gsub("^t","time", colnames(meanstd_dataset))
colnames(meanstd_dataset)<-gsub("^f","freq", colnames(meanstd_dataset))
colnames(meanstd_dataset)<-gsub("\\(","", colnames(meanstd_dataset))
colnames(meanstd_dataset)<-gsub(")","", colnames(meanstd_dataset))
colnames(meanstd_dataset)<-gsub("-","", colnames(meanstd_dataset))
##END GOAL (4): Appropriately labels the data set with descriptive variable names.

##GOAL (5): From the data set in step 4, creates a second, independent tidy data set with the 
##average of each variable for each activity and each subject.
##Load dplyr package
library(dplyr)

##Make new data set with grouping and mean calc
tidy_dataset <- meanstd_dataset %>% group_by(subject, activity) %>% summarise_all(.funs = "mean")

##Export the result
write.table(tydy_dataset, "tidy_dataset.txt", row.names = FALSE)

##END GOAL (5): From the data set in step 4, creates a second, independent tidy data set with the 
##average of each variable for each activity and each subject.


