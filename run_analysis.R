##  Coursera Course Data Analyst, Clean Data Week 4 Course Project
##  please consult the Code book and ReadMe document  to get insights
##  

 fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
 download.file(fileUrl,destfile="./RAW_Data.zip",method="curl")


###Unzip DataSet to /data directory
 unzip(zipfile="./RAW_Data.zip",exdir="./")


###Load required packages
library(dplyr)
library(data.table)
library(tidyr)

ls()
filesPath <- "./UCI HAR Dataset"
# Read subject files
dataSubTrain <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
dataSubTest  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))

# Read activity files
dataActTrain <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
dataActTest  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))

#Read data files.
dataTrain <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))

# Reading features.txt
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))

# Reading features.txt and extracting only the mean and standard deviation
 # dataFeatMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name

ls()

# for both Activity and Subject files this will merge the training and the test sets by row binding
#and rename variables "subject" and "activityNum"
alldataSub <- rbind(dataSubTrain, dataSubTest)
setnames(alldataSub, "V1", "subject")
alldataAct<- rbind(dataActTrain, dataActTest)
setnames(alldataAct, "V1", "activityNum")

#combine the DATA training and test files
dataTable <- rbind(dataTrain, dataTest)

# name variables according to feature e.g.(V1 = "tBodyAcc-mean()-X")

setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

#column names for activity labels
activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Merge columns
alldataSubjAct<- cbind(alldataSub, alldataAct)
dataTable <- cbind(alldataSubjAct, dataTable)

# Taking only measurements for the mean and standard deviation and add "subject","activityNum"
 dataFeatMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name 
 dim(dataFeatMeanStd)
 dataFeatMeanStd <- union(c("subject","activityNum"), dataFeatMeanStd)
 dim(dataFeatMeanStd)
 data<- subset(dataTable,select=dataFeatMeanStd)
 dim(data)
##enter name of activity into data
 data <- merge(activityLabels, data , by="activityNum", all.x=TRUE)
 data$activityName <- as.character(data$activityName)
 dim(data)
## create dataTable with variable means sorted by subject and Activity
 data$activityName <- as.character(data$activityName)
 dataAgg<- aggregate(. ~ subject - activityName, data = data, mean)
 dim(dataAgg)
 Tidydata<- tbl_df(arrange(dataAgg,subject,activityName))
 dim(Tidydata)
#Names before
   head(str(Tidydata),2)

 names(Tidydata)<-gsub("std()", "SD", names(Tidydata))
 names(Tidydata)<-gsub("mean()", "MEAN", names(Tidydata))
 names(Tidydata)<-gsub("^t", "time", names(Tidydata))
 names(Tidydata)<-gsub("^f", "frequency", names(Tidydata))
 names(Tidydata)<-gsub("Acc", "Accelerometer", names(Tidydata))
 names(Tidydata)<-gsub("Gyro", "Gyroscope", names(Tidydata))
 names(Tidydata)<-gsub("Mag", "Magnitude", names(Tidydata))
 names(Tidydata)<-gsub("BodyBody", "Body", names(Tidydata))
 # Names after
  head(str(Tidydata),6)

##write to text file on disk
 write.table(Tidydata, "TidyData.txt", row.name=FALSE)
 

