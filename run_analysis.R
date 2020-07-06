
#####
## install.packages("dplyr")
library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
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
## Files are downloaded and the following files exist
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


neededfiles <- c(file_activitylabels, file_features, file_testsubject,
                 file_testvariables, file_testactivity, file_trainsubject,
                 file_trainvariables, file_trainactivity)
sapply(neededfiles, function(f) if(!file.exists(f)) stop(paste("Needed file ", f, " doesn't exist. Exitting ...", sep="")))

####
## Read file_features and fix duplicate
features <- read.table(file_features, col.names=c("rownumber","variablename"))
####

####
## Fix the issue with duplicate names (e.g.) 516. fBodyBodyAccJerkMag-mean()
allvariables <- 
  mutate(features, variablename = gsub("BodyBody", "Body", variablename))
####

####
## Filter the 66 variables - mean() and std()
####
requestedvariables <- filter(allvariables, grepl("mean\\(\\)|std\\(\\)", variablename))

####
## Make the allvariables readable
##    Remove special characters, Convert to lower case
####
allvariables <- mutate(allvariables, variablename = gsub("-", "", variablename),
                       variablename = gsub("\\(", "", variablename),
                       variablename = gsub("\\)", "", variablename),
                       variablename = tolower(variablename))

####
## Make the requestedvariables readable
##    Remove special characters, Convert to lower case
####
requestedvariables <- mutate(requestedvariables, variablename = gsub("-", "", variablename),
                          variablename = gsub("\\(", "", variablename),
                          variablename = gsub("\\)", "", variablename),
                          variablename = tolower(variablename))

####
## Read activitylabelsfile
activitylabels <- read.table(file_activitylabels, col.names=c("activity", "activitydescription"))
####

####
## Read in test data stats
####
test_data <- read.table(file_testvariables, col.names = allvariables$variablename)
requiredtestdata <- test_data[ , requestedvariables$variablename]
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
testactivitieswithdescr <- merge(testactivities, activitylabels)
####

####
## Put the test data together
##    Assuming that the data is in the same order and all we need is cbind
##    Combining values, activities, subjects
testdata <- cbind(testactivitieswithdescr, testsubjects, testneededvalues)
####

####
## Read in train variables
####
traindata <- read.table(file_trainvariables, col.names = allvariables$variablename)
requiredtraindata <- traindata[ , requestedvariables$variablename]
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
trainactivitieswithdescr <- merge(trainactivities, activitylabels)
####

####
## Put the train data together
##    Assuming that the data is in the same order and all we need is cbind
##    Combining values, activities, subjects
traindata <- cbind(trainactivitieswithdescr, trainsubjects, requiredtraindata)
####

####
## Combine the testdata and traindata
## Additionally make subject a factor
combined_test_train_data <- rbind(testdata, traindata) %>% select( -activity )
combined_test_train_data <- mutate(combined_test_train_data, subject = as.factor(combined_test_train_data$subject))
####

####
## Write the data out
write.table(combined_test_train_data, "Mean_And_StdDev_For_Activity_Subject.txt")
####

####
## Create a second, independent tidy data set with the average of each 
##        variable for each activity and each subject.
## Group the data by activity, subject
grouped_combined_data <- group_by(combined_test_train_data,activitydescription,subject)
## Get the average of each variable
summarised_combined_data <- summarise_each(grouped_combined_data, funs(mean))
## Write the data out
write.table(summarised_combined_data, "Average_Variable_By_Activity_Subject.txt", row.names = FALSE)
####