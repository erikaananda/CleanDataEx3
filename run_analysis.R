#You should create one R script called run_analysis.R that does the following. 

#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement. 
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names. 

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#install.packages("stringr")
library(stringr)
#setwd("./R/CleaningData/Week3/UCI HAR Dataset/")
aLabels <- read.table("activity_labels.txt")
features <- read.table("features.txt")
#head(features)
#dim(features)

#get train data, labels, subjects
labels <- read.table("./train/y_train.txt")
xData <- read.table("./train/X_train.txt")
subjData <- read.table("./train/subject_train.txt")

#add a description column to each label
mLabels <- merge(labels, aLabels)
#name the subject column
names(subjData) <- "Subject"
#name the activity columns
names(mLabels) <- c("ActivityID", "ActivityName")
dim(xData)
#name the data/measure columns
names(xData) <- features[, 2]
#head(xData[, 1:5])

xData <- cbind(subjData, mLabels, xData)

#get test data, label, subjects
testLabels <- read.table("./test/y_test.txt")
testData <- read.table("./test/X_test.txt")
subjTestData <- read.table("./test/subject_test.txt")
#dim(testData)
#dim(testLabels)

#add a column descriptions
mTestLabels <- merge(testLabels, aLabels)
#head(mTestLabels)
names(mTestLabels) <- c("ActivityID", "ActivityName")
names(testData) <- features[, 2]
names(subjTestData) <- "Subject"

testData <- cbind(subjTestData, mTestLabels, testData)
names(testData)
combData <- rbind(xData, testData)
#dim(combData)
#names(combData)

# find columns that contain means or stds
meanIndex <- str_locate(names(combData), "-mean()")
stdIndex <- str_locate(names(combData), "-std()")
combIndex <- !(is.na(meanIndex)) | !(is.na(stdIndex))
# make sure we include the subjecct, activity ID and name
combIndex[1:3] <- TRUE

# create a data frame of just mean and std measurements
combMeanStdData <- combData[, combIndex[, 1]]

avgData <- group_by(combMeanStdData, Subject, ActivityID, ActivityName ) %>% summarize_each(funs(mean))
write.table(avgData, file = "avg_data_wk3.txt",row.name=FALSE)
