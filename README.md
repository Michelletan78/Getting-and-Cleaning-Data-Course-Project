# Getting-and-Cleaning-Data-Course-Project
Getting and Cleaning Data Course week 4 project assignment 



#Review Criteria #1. The submitted data set is tidy. #2. The Github repo contains the required scripts. #3. GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information. #4. The README that explains the analysis files is clear and understandable. #5. The work submitted for this project is the work of the student who submitted it.

#Create one R script called run_analysis.R that does the following.

#1. Merges the training and the test sets to create one data set. #2. Extracts only the measurements on the mean and standard deviation for each measurement. #3. Uses descriptive activity names to name the activities in the data set #4. Appropriately labels the data set with descriptive variable names. #5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#1. Merges the training and the test sets to create one data set.

#Install the packages that will help acheive these goals:

library(dplyr)  library(data.table)
Loading raw data sets.

#read in the data sets from UCI HAR Dataset, found here: #https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
setwd("~/R-Programming_DataScience_Coursera/UCI HAR Dataset")
  library(plyr)
  library(data.table)
subjecTrain = read.table('./train/subject_train.txt', header = FALSE)
xTrain= read.table('./train/x_train.txt',header=FALSE)
yTrain= read.table('./train/y_train.txt',header=FALSE)
subjectTest = read.table('./test/subject_test.txt', header = FALSE)
xTest = read.table('./test/x_test.txt',header = FALSE)
yTest = read.table('./test/y_test.txt',header = FALSE)
xDataSet <- rbind(xTrain, xTest)
yDataSet <- rbind(yTrain, yTest)
subjectDataSet <- rbind(subjecTrain,subjectTest)


#2. Extracts only the measurements on the mean and standard deviation for each measurement.
  
xDataSet_mean_std <- xDataSet[, grep("-(mean|std)\\(\\)", read.table("features.txt") [,2])]
names(xDataSet_mean_std) <- read.table("features.txt")[grep("-(mean|std)\\(\\)", read.table("features.txt") [, 2]), 2]
View(xDataSet_mean_std)
dim(xDataSet_mean_std)

#3. Use descriptive activity names to name the activities in the data set.
yDataSet[, 1] <- read.table("activity_labels.txt")[yDataSet[, 1], 2]
names(yDataSet) <- "Activity"
View(yDataSet)

#4. Appropriately labels the data set with descriptive variable names.

names(subjectDataSet) <- "Subject"
summary(subjectDataSet)


#5. From the data set in step 4, creates a second, independent tidy data set with the #average of each variable for each activity and each subject.
singleDataSet <- cbind(xDataSet_mean_std, yDataSet, subjectDataSet)

# Defining descriptive names for all variables.

names(singleDataSet) <- make.names(names(singleDataSet))
names(singleDataSet) <- gsub('Acc', "Acceleration", names(singleDataSet))
names(singleDataSet) <- gsub('GyroJerk', "AngularAcceleration", names(singleDataSet))
names(singleDataSet) <- gsub('Gyro',"AngularSpeed", names(singleDataSet))
names(singleDataSet) <- gsub('Mag',"Magnitude", names(singleDataSet))
names(singleDataSet) <- gsub('^t', "TimeDomain.",names(singleDataSet))
names(singleDataSet) <- gsub('^f', "FrequencyDomain.", names(singleDataSet))
names(singleDataSet) <- gsub('\\.mean',".Mean",names(singleDataSet))
names(singleDataSet) <- gsub('\\.std',".StandardDeviation",names(singleDataSet))
names(singleDataSet) <- gsub('Freq\\.',"Frenquency.",names(singleDataSet))
names(singleDataSet) <- gsub('Freq$',"Frenquency",names(singleDataSet))

View(singleDataSet)


names(singleDataSet)

Data2 <- aggregate(. ~Subject + Activity, singleDataSet, mean)
Data2 <- Data2[order(Data2$Subject,Data2$Activity),]
write.table(Data2, file="tidytata.txt", row.names = FALSE)
