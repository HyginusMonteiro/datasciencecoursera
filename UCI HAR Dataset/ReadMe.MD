# Overview
Course Project for Getting and Cleaning Data.
This read my file explains the logic used to create run_analysis.R program.
Author: Hyginus Monteiro
Date: 26-Oct-2014

# Project Backgroud
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
The goal is to prepare tidy data that can be used for later analysis. 

# Project Prerequists
Download file in Local Working Directory.
File Source: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
Set working directory to directory where all the downloaded files are stored (UCI HAR Dataset)

# Project Details
## Objectives 
1.Merges the training and the test sets to create one data set.
2.Extracts only the measurements on the mean and standard deviation for each measurement. 
3.Uses descriptive activity names to name the activities in the data set
4.Appropriately labels the data set with descriptive variable names. 
5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

### Logic for section 1
1.Read data from files and assing appropriate col names.
2.Merge Data in single vector mergTestTrainingData

### Logic for section 2
1.Create a logical vector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
2.Subset merged file based on above condition.

### Logic for section 3
1.Merge the mergTestTrainingData set with the acitivityType table to include descriptive activity names


###Logic for section 4
1.Clean up the variable names
2.Reassigning the new descriptive column names to the mergTestTrainingData set

### Logic for section 5
1.Create a new table, mergTestTrainingData_2 without the activityType column
2.Summarizing the mergTestTrainingData_2 table to include just the mean of each variable for each activity and each subject
3.Merging the tidyData with activityType to include descriptive acitvity names
4.Export the tidyData set 

