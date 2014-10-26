## -------------------------------------------------------------------------------------------------------
## -------------------------------------------------------------------------------------------------------
## Program Name: run_analysis.R
## Author: Hyginus Monteiro
## Date: 19-Oct-2014
## -------------------------------------------------------------------------------------------------------
## -------------------------------------------------------------------------------------------------------
## Prerequists
## Download file in Local Working Directory.
## File Source: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## Set working directory to directory where all the downloaded files are stored (UCI HAR Dataset)
## -------------------------------------------------------------------------------------------------------
## -------------------------------------------------------------------------------------------------------
## Program Starts
## -------------------------------------------------------------------------------------------------------

## ------------------------------------Section 1 Starts ---------------------------------------------------------------------
## -------------------Merges the training and the test sets to create one data set-------------------------------------------

## Reading all files.
file_features = read.table('./features.txt',header=FALSE)
file_activity_labels = read.table('./activity_labels.txt',header=FALSE)
file_subject_train = read.table('./train/subject_train.txt',header=FALSE)
file_x_train = read.table('./train/x_train.txt',header=FALSE)
file_y_train = read.table('./train/y_train.txt',header=FALSE)


## Provide Col Names to file
colnames(file_activity_labels) = c('activityId','activityType')
colnames(file_subject_train) = "subjectId"
colnames(file_x_train) = file_features[,2]
colnames(file_y_train) = "activityId"


## Read in the test data
file_subject_test = read.table('./test/subject_test.txt',header=FALSE)
file_x_test = read.table('./test/x_test.txt',header=FALSE)
file_y_test = read.table('./test/y_test.txt',header=FALSE)


## Provide Col Names to file
colnames(file_subject_test) = "subjectId"
colnames(file_x_test) = file_features[,2]
colnames(file_y_test) = "activityId"

## Merge Data
mergeTestData = cbind(file_y_test,file_subject_test,file_x_test)
mergeTrainingData = cbind(file_y_train,file_subject_train,file_x_train)
mergTestTrainingData = rbind(mergeTrainingData,mergeTestData)


## Get col names of merged file 
file_col_names = colnames(mergTestTrainingData)

## ------------------------------------Section 1 Ends -----------------------------------------------------------------------

## ------------------------------------Section 2 Starts ---------------------------------------------------------------------
## -------------------Extracts only the measurements on the mean and standard deviation for each measurement ----------------


## Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
Check_Output = (grepl("activity..",file_col_names) | grepl("subject..",file_col_names) | grepl("-mean..",file_col_names) & !grepl("-meanFreq..",file_col_names) & !grepl("mean..-",file_col_names) | grepl("-std..",file_col_names) & !grepl("-std()..-",file_col_names))

## Subset merged file based on above condition.
mergTestTrainingData = mergTestTrainingData[Check_Output==TRUE]

## ------------------------------------Section 2 Ends ----------------------------------------------------------------------

## ------------------------------------Section 3 Starts ---------------------------------------------------------------------
## -----------------Uses descriptive activity names to name the activities in the data set ----------------------------------


mergTestTrainingData = merge(mergTestTrainingData,file_activity_labels,by='activityId',all.x=TRUE)
file_col_names = colnames(mergTestTrainingData)
## ------------------------------------Section 3 Ends ----------------------------------------------------------------------


## ------------------------------------Section 4 Starts ---------------------------------------------------------------------
## -------------------Appropriately label the data set with descriptive activity names.  ------------------------------------

for (i in 1:length(file_col_names))
{
file_col_names[i] = gsub("\\()","",file_col_names[i])
file_col_names[i] = gsub("-std$","StdDev",file_col_names[i])
file_col_names[i] = gsub("-mean","Mean",file_col_names[i])
file_col_names[i] = gsub("^(t)","time",file_col_names[i])
file_col_names[i] = gsub("^(f)","freq",file_col_names[i])
file_col_names[i] = gsub("([Gg]ravity)","Gravity",file_col_names[i])
file_col_names[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",file_col_names[i])
file_col_names[i] = gsub("[Gg]yro","Gyro",file_col_names[i])
file_col_names[i] = gsub("AccMag","AccMagnitude",file_col_names[i])
file_col_names[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",file_col_names[i])
file_col_names[i] = gsub("JerkMag","JerkMagnitude",file_col_names[i])
file_col_names[i] = gsub("GyroMag","GyroMagnitude",file_col_names[i])
}
colnames(mergTestTrainingData) = file_col_names


## ------------------------------------Section 4 Ends ----------------------------------------------------------------------



## ------------------------------------Section 5 Starts ---------------------------------------------------------------------
## ---Create a second, independent tidy data set with the average of each variable for each activity and each subject -------

mergTestTrainingData_2 = mergTestTrainingData[,names(mergTestTrainingData) != 'activityType'];
tidyData = aggregate(mergTestTrainingData_2[,names(mergTestTrainingData_2) != c('activityId','subjectId')],by=list(activityId=mergTestTrainingData_2$activityId,subjectId = mergTestTrainingData_2$subjectId),mean)
tidyData = merge(tidyData,file_activity_labels,by='activityId',all.x=TRUE);
write.table(tidyData, './tidyData.txt',row.name=FALSE,sep='\t')

## ------------------------------------Section 5 Ends ----------------------------------------------------------------------