# Overview
Course Project for Getting and Cleaning Data.
This is Code Book for run_analysis.R Program.
Author: Hyginus Monteiro
Date: 26-Oct-2014

# Project Backgroud
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
The goal is to prepare tidy data that can be used for later analysis. 

# Project Prerequists
Download file in Local Working Directory.
File Source: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
Set working directory to directory where all the downloaded files are stored (UCI HAR Dataset)

# Input details
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.
The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. 


# Program Details
## Following is the highlevel logic of the program
1.Merges the training and the test sets to create one data set.
2.Extracts only the measurements on the mean and standard deviation for each measurement. 
3.Uses descriptive activity names to name the activities in the data set
4.Appropriately labels the data set with descriptive variable names. 
5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Variables created in the Program
file_features --> features data 
file_activity_labels --> activity data
file_subject_train --> sub train data
file_x_train --> x_train data
file_y_train --> y_train data
file_subject_test --> sub test data 
file_x_test --> x_test data
file_y_test --> y_test data
mergTestTrainingData --> merged test and training data with descriptive activity names. Also names have been cleansed.
file_col_names --> col names of mergTestTrainingData
mergTestTrainingData_2 --> merged data without activity type
tidyData --> Output data

#OutPut Data
## Output data contains the following elements
activityId
subjectId
timeBodyAccMagnitudeMean
timeBodyAccMagnitudeStdDev
timeGravityAccMagnitudeMean
timeGravityAccMagnitudeStdDev
timeBodyAccJerkMagnitudeMean
timeBodyAccJerkMagnitudeStdDev
timeBodyGyroMagnitudeMean
timeBodyGyroMagnitudeStdDev
timeBodyGyroJerkMagnitudeMean
timeBodyGyroJerkMagnitudeStdDev
freqBodyAccMagnitudeMean
freqBodyAccMagnitudeStdDev
freqBodyAccJerkMagnitudeMean
freqBodyAccJerkMagnitudeStdDev
freqBodyGyroMagnitudeMean
freqBodyGyroMagnitudeStdDev
freqBodyGyroJerkMagnitudeMean
freqBodyGyroJerkMagnitudeStdDev
activityType
