#PACKAGES NEEDED
library(plyr)

#1. MERGE DATA
#1.a - MERGE FEATURES DATA (X)
#1.a.I - Read data
test_x<-read.table(".\\UCI HAR Dataset\\test\\X_test.txt",sep="",nrows=2947)
train_x<-read.table(".\\UCI HAR Dataset\\train\\X_train.txt",sep="",nrows=7352)
features_names<- read.table(".\\UCI HAR Dataset\\features.txt",sep="")

#1.a.II - Merge them
features<-rbind(test_x,train_x)

#1.a.III - Rename columns
names(features)<-t(features_names[2])

#1.b- MERGE ACTIVITY DATA (Y)
#1.b.I - Read data
test_y<-read.table(".\\UCI HAR Dataset\\test\\Y_test.txt",sep="",nrows=2947)
train_y<-read.table(".\\UCI HAR Dataset\\train\\y_train.txt",sep="",nrows=7352)

#1.b.II - Merge them
activities<-rbind(test_y,train_y)

#1.b.III - Rename columns
names(activities)<-"Activity"

#1.c - MERGE SUBJECT DATA
#1.c.I - Read data
subject_test<-read.table(".\\UCI HAR Dataset\\test\\subject_test.txt",sep="",nrows=2947)
subject_train<-read.table(".\\UCI HAR Dataset\\train\\subject_train.txt",sep="",nrows=7352)

#1.c.II - Merge them
subject<-rbind(subject_test,subject_train)

#1.c.III - Rename columns
names(subject)<-"Subject"

#1.d - Join them all
all_data<-cbind(subject,activities,features)
write.csv(all_data,".\\run_analysis1-4.csv",row.names=FALSE)

#2 - Select only mean and std of the measures -> Interpretation: Select only those features
#    that contain mean() and std()
meanstd<-grep(".*mean.*|.*std.*",features_names$V2)
msdata<-features[,meanstd]
all_data_mstd<-cbind(subject, activities, msdata)
write.csv(all_data_mstd,".\\run_analysis1-4.csv",row.names=FALSE)

#3 - Uses descriptive activity names to name the activities in the data set .> activities
#    names instead of only numbers, using activity_labels vs. test_y/train_y
activities_names<- read.table(".\\UCI HAR Dataset\\activity_labels.txt",sep="")
activities_names<-rename(activities_names,replace=c("V1"="Activity"))
activities_names<-rename(activities_names,replace=c("V2"="Activity_name"))

activities_with_names<-join(activities,activities_names,by="Activity")

all_data_mstd_act<-cbind(subject, activities_with_names, msdata)

write.csv(all_data_mstd_act,".\\run_analysis1-4.csv",row.names=FALSE)

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

write.csv(all_data_mstd_act,".\\run_analysis1-4.csv",row.names=FALSE)

#5. From the data set in step 4, creates a second, independent tidy data set with the 
#   average of each variable for each activity and each subject.
#5.a - Grouping by Subject and Activity_name, get the average of all the Features
summary_grouped<-aggregate(. ~Subject + Activity_name,all_data_mstd_act,mean)
#5.b - Create the independent document
write.table(summary_grouped,".\\run_analysis5.txt",row.names=FALSE)
