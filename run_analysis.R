#This is my coursera assignment for the course "Getting and Cleaning Data"

library(dplyr)

#To run this R script please make sure that you have downloaded the zip file from the link given in the assignment page.
#download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "UCI HAR Dataset.zip")

#Extract the contents of the file into working directory
#unzip("UCI HAR Dataset.zip", list = FALSE,exdir = getwd())

#Part 1 - Merges the test and training datasets to create one data set.

#Load both the test and training measurements datasets on to R
data_test <- read.table("UCI HAR Dataset/test/X_test.txt")
data_train <- read.table("UCI HAR Dataset/train/X_train.txt")

#Load subject ids and activities recorded in each measurement
data_test_subject <- readLines("UCI HAR Dataset/test/subject_test.txt")
data_train_subject <- readLines("UCI HAR Dataset/train/subject_train.txt")
data_test_activity <- readLines("UCI HAR Dataset/test/y_test.txt")
data_train_activity <- readLines("UCI HAR Dataset/train/y_train.txt")

#Merge the test and training datasets 
data_subject <- c(data_test_subject,data_train_subject)
data_activity <- c(data_test_activity,data_train_activity)
data_measurements <- rbind.data.frame(data_test,data_train)
data_merged <- cbind.data.frame(data_subject,data_activity,data_measurements)

#Part 2 - Extracts only the measurements on the mean and standard deviation for each measurement.

#Load the file containing features listed in each column.
features <- readLines("UCI HAR Dataset/features.txt")

#Select the indices of features containing only the mean and standard deviation. 
mean_and_std <- c(grep("mean()",features, fixed = TRUE),grep("std()",features, fixed = TRUE))

#Subset the data to contain only the mean and standard deviation of each measurement
data_subset <- data_merged[,c(1, 2, mean_and_std + 2)]

#Part 3 - Uses descriptive activity names to name the activities in the data set.

#Load the activity labels file and make it descriptive
activity <- readLines("UCI HAR Dataset/activity_labels.txt")
activity <- sapply(strsplit(activity, " "), function(x)x[2])

#Label the measurements with descriptive activity names
data_subset[[2]] <- factor(x = data_subset[[2]], labels = activity)

#Part 4 - Appropriately labels the data set with descriptive variable names.

#Create a character vector with variable names to be set to each column
variable_names <- c("SubjectID","Activity",
                    sapply(strsplit(features[mean_and_std], " "), function(x)x[2]))
variable_names <- gsub("-","_",variable_names)
variable_names <- gsub("\\()","",variable_names)
#Set names of the column to be ones in the vector
names(data_subset) <- variable_names

#Part 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Group data based on Subject ID and Activity
data_separate <- group_by(data_subset, SubjectID, Activity)

#Calculate mean of each measurement for each subject and each activity
data_final <- summarise_each_(data_separate, funs(mean), names(data_separate)[-(1:2)])

#Save the final data set as a .txt file
write.table(data_final,"Summarised Dataset.txt",row.names = FALSE)