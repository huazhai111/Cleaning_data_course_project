#Getting and Cleaning Data Course Project
#You should create one R script called run_analysis.R that does the following. 
#1.Merges the training and the test sets to create one data set.
#2.Extracts only the measurements on the mean and standard deviation 
#for each measurement. 
#3.Uses descriptive activity names to name the activities in the data set
#4.Appropriately labels the data set with descriptive variable names. 
#5.From the data set in step 4, creates a second, 
#independent tidy data set with the average of each variable 
#for each activity and each subject.



#read into R the desired data files(train, test)
x_train<-read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
y_train<-read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
names(y_train)<-"activityId"
names(subject_train)<-"subject"



x_test<-read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
y_test<-read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
names(y_test)<-"activityId"
names(subject_test)<-"subject"



#read reference data files
activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt")
names(activity_labels)<-c("activityId", "activityName")

features<-read.table("./UCI HAR Dataset/features.txt")

#Extracts only the features on the mean and standard deviation 
features_useful<-features[c(grep("*mean\\(\\)|*std\\(\\)", features[,2])),]

#Appropriately labels the data set with descriptive variable names
features_useful[,2]<-sub("tBodyAcc", "timeBodyAcceleration", features_useful[,2])
features_useful[,2]<-sub("tGravityAcc", "timeGravityAcceleration", features_useful[,2])
features_useful[,2]<-sub("tBodyAcc", "timeBodyAcceleration", features_useful[,2])
features_useful[,2]<-sub("tGravityAcc", "timeGravityAcceleration", features_useful[,2])
features_useful[,2]<-sub("tBodyGyro", "timeBodyGyroscope", features_useful[,2])
features_useful[,2]<-sub("fBodyAcc", "frequencyBodyAcceleration", features_useful[,2])
features_useful[,2]<-sub("fBodyGyro", "frequencyBodyGyroscope", features_useful[,2])
features_useful[,2]<-sub("Mag", "Magnitude", features_useful[,2])

#get the useful column id
colid<-features_useful[,1]
x_train_useful<-x_train[,c(colid)]
x_test_useful<-x_test[,c(colid)]

combined<-rbind(x_train_useful, x_test_useful)

#get the useful column header values
header<-features_useful[,2]
names(combined)<-c(header)


#add activityId column to data file
y_combined<-rbind(y_train, y_test)
combined<-cbind(y_combined, combined)

#add subject column to data file
subject_combined<-rbind(subject_train, subject_test)
combined<-cbind(subject_combined, combined)

#Uses descriptive activity names to name the activities in the data set
combined<-merge(activity_labels, combined, by="activityId")
#remove activityId column
combined<-combined[,-c(1)]

#5.From the data set in step 4, creates a second, 
#independent tidy data set with the average of each variable 
#for each activity and each subject.
tidy<-aggregate(. ~subject + activityName, combined, mean)
tidy_ordered<-tidy[order(tidy$subject, tidy$activityName),]

write.table(tidy_ordered, "tidy.txt", row.name=FALSE)


