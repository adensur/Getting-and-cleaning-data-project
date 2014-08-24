##The script creates 2 tidy data sets from the raw data, located at "UCI HAR Dataset" folder. 
##The "UCI HAR Dataset" folder has to be located in R working directory.


##Part 1. Corresponds to creating the first tidy data set for questions 1-4 in the assignement.


##First we load all the files we need into R. 
##We ignore all the "rawest" data, located in the "Inertial Sygnals" folder.

##global data
activity_labels<-read.table("UCI HAR Dataset/activity_labels.txt")   ##file with 6 activity names
features<-read.table("UCI HAR Dataset/features.txt")                 ##file with 561 feature names

##"train" data
X_train<-read.table("UCI HAR Dataset/train/X_train.txt")             ##file with main training data 
y_train<-read.table("UCI HAR Dataset/train/y_train.txt")             ##list of activity indexes for each measurement
subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt") ##list of subject number for each measurement

##"test" data
X_test<-read.table("UCI HAR Dataset/test/X_test.txt")                ##these variables are the same as above, but
y_test<-read.table("UCI HAR Dataset/test/y_test.txt")                ##for test data set
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt")

##Now we replace the activity indexes with activity names, i.e.we write,for example, "STANDING" instead of "5"
label<-function(i, labels){
        labels[i,2]
}##this function returns the activity label 
                               ##when passed the index and the variable name for data frame with label
y_train<-sapply(y_train,FUN=label,labels=activity_labels)##replaces all the activity indexes in train data with activity names
y_test<-sapply(y_test,FUN=label,labels=activity_labels)  ##replaces all the activity indexes in test data with activtiy names
names(X_train)<-features[,2]                             ##sets appropriate column names
names(X_test)<-features[,2]
index<-c(1:6,41:46,81:86,121:126,161:166,201,202,214,215,227,228,     ##this is a vector that chooses only the columns
    240,241,253,254,266:271,294:296,345:350,373:375,424:429,452:454,  ##that correspond to mean or standart deviation 
    503,504,513,516,517,526,529,530,539,542,543,552,555:561)          ##from the 561-long feature vector

X_train_short<-X_train[,index]                                        ##shortens the feature vector to include only values we need
X_test_short<-X_test[,index]
train_index<-rep("train",times=7352)                                  ##mark measurements in train data set with label "train"
test_index<-rep("test",times=2947)                                    ##mark measurements in test data set with label "test"
train<-cbind(subject_train,y_train,train_index,X_train_short)         ##connects the main data, activity labels and subject
names(train)<-c("subject","activity","test/train",names(X_train_short))##adds names for the first 3 columns
test<-cbind(subject_test,y_test,test_index,X_test_short)
names(test)<-c("subject","activity","test/train",names(X_test_short))
tidy_data<-rbind(train,test)                                          ##connects the train data and test data
write.table(tidy_data,file="Tidy Data/tidy_data.txt",row.names=FALSE)

##this is the end of part 1.

##Part 2. Creating a second data set for question 5.

##all the files are already loaded into R in the part 1.
##first we clip together ALL the data the same way we did before, but now we include
##full feature vector, not just shortened part of it.
train2<-cbind(subject_train,y_train,train_index,X_train)           ##clips together the labels, subject and feature vector
names(train2)<-c("subject","activity","test/train",names(X_train))
test2<-cbind(subject_test,y_test,test_index,X_test)
names(test2)<-c("subject","activity","test/train",names(X_test))
data2<-rbind(train2,test2)                                         ##clips together the test data and train data

means_matrix<-NULL                                                 ##means_matrix will be a resulting matrix of means
for(i in 1:30){                                                    ##i is an index for subject
        for(j in levels(data2$activity)){                          ##j is an index for activity
                y<-data2[data2$subject==i & data2$activity==j,]    ##selects a subset of data, including 1 subject and 1 activity
                y<-y[,4:564]                                       ##selects only the numeric data (in feature vector)
                y<-c(i,j,colMeans(y))                              ##calculate the means across all the measurements for
                means_matrix<-rbind(means_matrix,y)                ##given subject and activity
        }
}

tidy_data2<-means_matrix
write.table(tidy_data2,file="Tidy Data/tidy_data2.txt",row.names=FALSE)
