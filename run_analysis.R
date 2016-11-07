setwd("~/Dropbox/Coursera/Getting_and_Cleaning_Data/Assignment/")

library(dplyr)

#Extract X column names
features <- read.table(file = "UCI HAR Dataset/features.txt")
x_colnames <- features$V2 

#Combine test data frames 
test.subject <- read.table(file = "UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
test.x <- read.table(file = "UCI HAR Dataset/test/X_test.txt", col.names = x_colnames)
test.y <- read.table(file = "UCI HAR Dataset/test/y_test.txt", col.names = "activity")
df.test <- data.frame(test.subject, test.y, test.x)

#Combine training data frames
train.subject <- read.table(file = "UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
train.x <- read.table(file = "UCI HAR Dataset/train/X_train.txt", col.names = x_colnames)
train.y <- read.table(file = "UCI HAR Dataset/train/y_train.txt", col.names = "activity")
df.train <- data.frame(train.subject, train.y, train.x)

#1: Merges the training and the test sets to create one data set
tidy.data <- rbind(df.train, df.test) 

#2: Extracts only the measurements on the mean and standard deviation for each measurement
rg <- grep("mean|std", features$V2)
df.meansd <- tidy.data[, rg]

#3: Uses descriptive activity names to name the activities in the data set
df.activity <- read.table(file = "UCI HAR Dataset/activity_labels.txt")
tidy.data$activity <- df.activity[tidy.data$activity, 2]

#4: Appropriately labels the data set with descriptive variable names
labels <- names(tidy.data)
labels <- sub("\\()","",labels)
labels <- gsub("-",".",labels)
labels <- sub("^t","timedomain.",labels)
labels <- sub("^f","frequencydomain.",labels)
labels <- sub("Mag",".magnitude",labels)
labels <- sub("Jerk",".jerk",labels)
labels <- sub("Acc",".acceleration",labels)
labels <- gsub("Body","body",labels)
labels <- gsub("Gyro","gyro",labels)
labels <- gsub("Gravity","gravity",labels)
names(tidy.data) <- labels

#5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
tidy.data_2nd <- tidy.data %>% group_by(activity,subject) %>% summarise_each(funs(mean))


# write both datasets to files
write.table(tidy.data,"UCI HAR tidy data.txt",row.name=FALSE)
write.table(tidy.data_2nd,"UCI HAR subject activity means.txt",row.name=FALSE)