# Getting-and-Cleaning-Data-Course-Project

This document explains what each script does, and the relationship between them.

First of all, we need to download the package plyr, as some of these scripts are using the formulas in plyr.
###PACKAGES NEEDED
library(plyr)

The first task is "create one R script called run_analysis.R that does the following: 1. Merges the training and the test sets to create one data set"

As explained in the CodeBook, the raw data provided can be separated into 3 sets: the Activity data, the Subject data and the Features data. I have not included the Inertial data, as I understood that is not required in the present assignment. 
The reason why I preferred to join the data in these 3 different groups, instead of joining the test data and the train data separately, is because the next tasks seem to refer to these 3 different groups.

###1. MERGE DATA
###1.a - MERGE FEATURES DATA (X)
###1.a.I - Read data - In the following, I read the data that we need to set up the Features data, including the train and the test data, plus the names that belong to each variable included in the set

test_x<-read.table(".\\UCI HAR Dataset\\test\\X_test.txt",sep="",nrows=2947)
train_x<-read.table(".\\UCI HAR Dataset\\train\\X_train.txt",sep="",nrows=7352)
features_names<- read.table(".\\UCI HAR Dataset\\features.txt",sep="")

###1.a.II - Merge them- We merge the data fron test and train data

features<-rbind(test_x,train_x)

###1.a.III - Rename columns - To ensure that we can understand the data, we use the transposition function to state the names included in the features.txt data as the variable names of the merged data "features".

names(features)<-t(features_names[2])

###1.b- MERGE ACTIVITY DATA (Y) - We repeat what was done for Features, for Activity data
###1.b.I - Read data
test_y<-read.table(".\\UCI HAR Dataset\\test\\Y_test.txt",sep="",nrows=2947)
train_y<-read.table(".\\UCI HAR Dataset\\train\\y_train.txt",sep="",nrows=7352)

###1.b.II - Merge them
activities<-rbind(test_y,train_y)

###1.b.III - Rename columns - In this case, as we only have 1 column so far in this dataset, we will just change the name of the one variable to "Activity" to make it clearer.

names(activities)<-"Activity"

###1.c - MERGE SUBJECT DATA - We repeat what was done for Activity, for Subject data
###1.c.I - Read data
subject_test<-read.table(".\\UCI HAR Dataset\\test\\subject_test.txt",sep="",nrows=2947)
subject_train<-read.table(".\\UCI HAR Dataset\\train\\subject_train.txt",sep="",nrows=7352)

###1.c.II - Merge them
subject<-rbind(subject_test,subject_train)

###1.c.III - Rename columns
names(subject)<-"Subject"

###1.d - Join them all - Now that we have all the data separated in the 3 groups (Features, Subject and Activity), we merge the data. As the observations are the same, we just need to bind the columns one to another.

all_data<-cbind(subject,activities,features)


###2 - Select only mean and std of the measures -> Interpretation: Select only those features
#    that contain mean() and std()
meanstd<-grep(".*mean.*|.*std.*",features_names$V2)
msdata<-features[,meanstd]
all_data_mstd<-cbind(subject, activities, msdata)

#3 - Uses descriptive activity names to name the activities in the data set .> activities
#    names instead of only numbers, using activity_labels vs. test_y/train_y
activities_names<- read.table(".\\UCI HAR Dataset\\activity_labels.txt",sep="")
activities_names<-rename(activities_names,replace=c("V1"="Activity"))
activities_names<-rename(activities_names,replace=c("V2"="Activity_name"))

activities_with_names<-join(activities,activities_names,by="Activity")

all_data_mstd_act<-cbind(subject, activities_with_names, msdata)

#4 - Appropriately labels the data set with descriptive variable names.
#4.a - Activities and Subjects are already descriptive
#4.b - Label the features columns
names(all_data_mstd_act)<-gsub("^t", "Time ",names(all_data_mstd_act))
names(all_data_mstd_act)<-gsub("Acc", "Accelerometer ",names(all_data_mstd_act))
names(all_data_mstd_act)<-gsub("Gyro", "Gyroscope ",names(all_data_mstd_act))
names(all_data_mstd_act)<-gsub("^f", "Frequency ",names(all_data_mstd_act))
names(all_data_mstd_act)<-gsub("Jerk", "Jerk signals ",names(all_data_mstd_act))
names(all_data_mstd_act)<-gsub("Mag", "Magnitude ",names(all_data_mstd_act))
names(all_data_mstd_act)<-gsub("Body", "Body ",names(all_data_mstd_act))
names(all_data_mstd_act)<-gsub("Body Body ", "Body ",names(all_data_mstd_act))
names(all_data_mstd_act)<-gsub("Gravity", "Gravity ",names(all_data_mstd_act))
names(all_data_mstd_act)<-gsub("mean()", "Mean ",names(all_data_mstd_act))
names(all_data_mstd_act)<-gsub("std()", "Standard Deviation ",names(all_data_mstd_act))

#5. From the data set in step 4, creates a second, independent tidy data set with the 
#   average of each variable for each activity and each subject.
summary_grouped<-aggregate(. ~Subject + Activity_name,all_data_mstd_act,mean)


#5. From the data set in step 4, creates a second, independent tidy data set with the 
#   average of each variable for each activity and each subject.
summary_grouped<-aggregate(. ~Subject + Activity_name,all_data_mstd_act,mean)
