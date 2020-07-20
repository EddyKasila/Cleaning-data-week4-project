#####
## install.packages("dplyr")
library(dplyr)
## 

#####
## Downloading the data and preparing the data
## Use the directory ".data' as working directory
datadirectory <- "./data"
if(!dir.exists("./data")) dir.create("./data")
setwd(datadirectory)

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipfile <- "UCI HAR Dataset.zip"
download.file(downloadurl, zipfile)

if(file.exists(zipfile)) unzip(zipfile)

####
## Read in files in the unzipped directory
##
parentdirectory <- "UCI HAR Dataset"
file_activitylabels <- paste(parentdirectory, "activity_labels.txt", sep="/")
file_features <- paste(parentdirectory, "features.txt", sep="/")
file_testsubject <- paste(parentdirectory, "test/subject_test.txt", sep="/")
file_testvariables <- paste(parentdirectory, "test/X_test.txt", sep="/")
file_testactivity <- paste(parentdirectory, "test/y_test.txt", sep="/")
file_trainsubject <- paste(parentdirectory, "train/subject_train.txt", sep="/")
file_trainvariables <- paste(parentdirectory, "train/X_train.txt", sep="/")
file_trainactivity <- paste(parentdirectory, "train/y_train.txt", sep="/")

####
## Read file_features and fix duplicated names
features <- read.table(file_features, col.names=c("rownumber","variablename")) %>%
  mutate(variablename = gsub("BodyBody", "Body", variablename)) 
####

####
## Filter to get the 66 variables of mean() and std()
####
requiredfeatures <- filter(features, grepl("mean\\(\\)|std\\(\\)", variablename))

####
## Make the features readable + Remove special characters, Convert to lower case
####
features <- features %>%
  mutate(variablename = gsub("-", "", variablename),
         variablename = gsub("\\(", "", variablename),
         variablename = gsub("\\)", "", variablename),
         variablename = tolower(variablename))

####
## Fix requiredfeatures: Remove special characters, Convert to lower case
####
 requiredfeatures <- requiredfeatures %>%
   mutate(variablename = gsub("-", "", variablename),
          variablename = gsub("\\(", "", variablename),
          variablename = gsub("\\)", "", variablename),
          variablename = tolower(variablename))

####
## Read activity labels
activitylabels <- read.table(file_activitylabels, col.names=c("activity", "activitydescription"))
####

####
## Read in test data stats
####
test_data <- read.table(file_testvariables, col.names = features$variablename)
requiredtestdata <- test_data[ , requiredfeatures$variablename]
####

## Read in test activities
testactivities <- read.table(file_testactivity, col.names=c("activity"))
####

####
## Read in test subjects
testsubjects <- read.table(file_testsubject, col.names=c("subject"))
####

####
## Add a readable activity description
described_testactivities <- merge(testactivities, activitylabels)
####

####
## Putting the test data together
####
testdata <- cbind(described_testactivities, testsubjects, requiredtestdata)
####

####
## Read in train variables
####
traindata <- read.table(file_trainvariables, col.names = features$variablename)
requiredtraindata <- traindata[ , requiredfeatures$variablename]
####

## Read in train activities
trainactivities <- read.table(file_trainactivity, col.names=c("activity"))
####

####
## Read in train subjects
trainsubjects <- read.table(file_trainsubject, col.names=c("subject"))
####

####
## Add a readable activity description
described_trainactivities <- merge(trainactivities, activitylabels)
####

####
## Putting the train data together
####
traindata <- cbind(described_trainactivities, trainsubjects, requiredtraindata)
####

####
## Combine testdata and traindata + make subject a factor
combined_test_train_data <- rbind(testdata, traindata) %>% select( -activity ) %>%
  mutate(subject = as.factor(subject))
####

####
## Write the data out
write.table(combined_test_train_data, "Mean_And_StdDeviation_For_Activity_Subject.txt")
####

####
## Create a second, independent tidy data set with the average of each 
##        variable for each activity and each subject.
## Group the data by activity, subject
summarized_grouped_combined_data <- combined_test_train_data %>% 
  group_by(activitydescription,subject) %>%
  summarise_each(funs(mean))
## Write the data out
write.table(summarized_grouped_combined_data, "Average_Variable_By_Activity_Subject.txt", row.names = FALSE)
