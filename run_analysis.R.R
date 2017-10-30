# Downloading and unzipping dataset 
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")


## Coursera Getting and Cleaning Data Week 4 Course Project
## Ralph C Jones 
## 2017-10-29

# RunAnalysis.R File Description:

# This script will perform the following steps on the UCI HAR Dataset  
# Data for the project https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

setwd("~/Coursera/Getting and Cleaning Data Course Project")

#Sourcing files

# Sourcing testing tables:
  x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
  y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
  subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
  
# Sourcing trainings tables:
  x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
  y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
  subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

# Sourcing Feature Vector and Activity Lables 
  features <- read.table('./data/UCI HAR Dataset/features.txt')
  activityLabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')

# Create testing column names 
  colnames(x_test) <- features[,2] 
  colnames(y_test) <- "activityId"
  colnames(subject_test) <- "subjectId"
  
# Create trainign column names 
  colnames(xtrain) <- features[,2] 
  colnames(y_Train) <-"activityId"
  colnames(subject_Train) <- "subjectId"

colnames(activityLabels) <- c('activityId','activityType')
  
# 1. Merging data into one dataset
  merg_train <- cbind(y_train, subject_train, x_train)
  merg_test <- cbind(y_test, subject_test, x_test)
  merged_sets<- rbind(merg_train, merg_test)
 
# Column names 
   colNames <- colnames(merged_sets)
   
   
# 2. Extract only the measurements on the mean and standard deviation for each measurement.
  mean_and_std <- grepl("activityId" , colNames)| 
                   grepl("subjectId" , colNames)|  
                   grepl("mean.." , colNames)|  
                   grepl("std.." , colNames)

#subset from merged_sets:
  merged_setMean_Std <- merged_sets[ , mean_and_std == TRUE ]
  
#3. Using descriptive activity names to name the activities in the data set:
  desc_activitynames <- merge(merged_setMean_Std, activityLabels, by='activityId', all.x=TRUE)

#4. Appropriately label the data set with descriptive activity names
  merg_train <- cbind(y_train, subject_train, x_train)
  merg_test <- cbind(y_test, subject_test, x_test)
  merged_sets<- rbind(merg_train, merg_test)
  grepl("activityId" , colNames)| 
    grepl("subjectId" , colNames)|  
    grepl("mean.." , colNames)|  
    grepl("std.." , colNames)
  merged_setMean_Std <- merged_sets[ , mean_and_std == TRUE ]
  
  
#5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.  

  # Second indepedent tidy data set 
    tidy_data <- aggregate(. ~subjectId + activityId, desc_activitynames, mean)
    tidy_data <- tidy_data [order(tidy_data$subjectId, tidy_data$activityId),] 
  
  # Second idependent tidy data set in txt file
    write.table(tidy_data, "tidy_data", row.name=FALSE)  
  