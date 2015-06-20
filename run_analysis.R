# Part 1 - Merge the training and test sets to create one data set

# Read initial data
x_train <- read.table("C:/Users/Dexter/Desktop/R/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("C:/Users/Dexter/Desktop/R/UCI HAR Dataset/train/Y_train.txt")
subject_train <- read.table("C:/Users/Dexter/Desktop/R/UCI HAR Dataset/train/subject_train.txt")

x_test <- read.table("C:/Users/Dexter/Desktop/R/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("C:/Users/Dexter/Desktop/R/UCI HAR Dataset/test/Y_test.txt")
subject_test <- read.table("C:/Users/Dexter/Desktop/R/UCI HAR Dataset/test/subject_test.txt")

features <- read.table("C:/Users/Dexter/Desktop/R/UCI HAR Dataset/features.txt")

# Rename variables
colnames(subject_train) <- "subject"
colnames(subject_test) <- "subject"

colnames(y_train) <- "activity"
colnames(y_test) <- "activity"

colnames(x_train) <- features[,2]
colnames(x_test) <- features[,2]

# Merge data sets
data_train <- cbind(subject_train,y_train,x_train)
data_test <- cbind(subject_test,y_test,x_test)

data <- rbind(data_train,data_test)

# Part 2 - Extracts only the measurements on the mean and 
# standard deviation for each measurement

cols <- names(data)

# Find subset of data containing mean and std data
meanCols <- grep("mean\\(\\)", cols)
meanData <- data[,meanCols]

stdCols <- grep("std\\(\\)", cols)
stdData <- data[,stdCols]

# Create new data set
subData <- cbind(data[,c(1,2)],meanData,stdData)

# Part 3 - Use descriptive activity names to name the activities in the data set.

# Replace labels with their corresponding descriptions
activity_labels <- read.table("C:/Users/Dexter/Desktop/R/UCI HAR Dataset/activity_labels.txt")
subData$activity <- sub(1, "WALKING", subData$activity)
subData$activity <- sub(2, "WALING_UPSTAIRS", subData$activity)
subData$activity <- sub(3, "WALKING_DOWNSTAIRS", subData$activity)
subData$activity <- sub(4, "SITTING", subData$activity)
subData$activity <- sub(5, "STANDING", subData$activity)
subData$activity <- sub(6, "LAYING", subData$activity)

# Part 4 - Appropriately labels the data set with descriptive variable names

# Clean up variable names
names(subData) <- gsub("-","",names(subData))
names(subData) <- gsub("\\(\\)","",names(subData))

# Part 5 - From the data set in step, creates a second, independent tidy data set with
# with the average of each variable for each activity and each subject.

library(plyr)

avgData <- ddply(subData,.(subject,activity), function(x) colMeans(x[,3:68]))
write.table(avgData, "avgData.txt", row.name=FALSE)
