# Load required libraries
library(reshape2)

# Step 1: Download and unzip the dataset
filename <- "getdata_dataset.zip"
if (!file.exists(filename)) {
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method = "curl")
}

if (!file.exists("UCI HAR Dataset")) {
  unzip(filename)
}

# Step 2: Load activity labels and features
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
activityLabels[, 2] <- as.character(activityLabels[, 2])
features <- read.table("UCI HAR Dataset/features.txt")
features[, 2] <- as.character(features[, 2])

# Step 3: Extract only the data on mean and standard deviation
featuresWanted <- grep(".*mean.*|.*std.*", features[, 2])
featuresWanted.names <- features[featuresWanted, 2]
featuresWanted.names <- gsub('-mean', 'Mean', featuresWanted.names)
featuresWanted.names <- gsub('-std', 'Std', featuresWanted.names)
featuresWanted.names <- gsub('[-()]', '', featuresWanted.names)

# Step 4: Load training and test datasets
# Load training data
train <- read.table("UCI HAR Dataset/train/X_train.txt")[featuresWanted]
trainActivities <- read.table("UCI HAR Dataset/train/Y_train.txt")
trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
train <- cbind(trainSubjects, trainActivities, train)

# Load test data
test <- read.table("UCI HAR Dataset/test/X_test.txt")[featuresWanted]
testActivities <- read.table("UCI HAR Dataset/test/Y_test.txt")
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
test <- cbind(testSubjects, testActivities, test)

# Step 5: Merge datasets and add descriptive column names
allData <- rbind(train, test)
colnames(allData) <- c("subject", "activity", featuresWanted.names)

# Step 6: Turn activities and subjects into factors
allData$activity <- factor(allData$activity, levels = activityLabels[, 1], labels = activityLabels[, 2])
allData$subject <- as.factor(allData$subject)

# Step 7: Reshape the data and calculate the average for each variable
# Reshape the data using melt
allData.melted <- melt(allData, id = c("subject", "activity"))

# Calculate the average for each variable
allData.mean <- reshape2::dcast(allData.melted, subject + activity ~ variable, fun.aggregate = function(x) mean(x, na.rm = TRUE))

# Write the tidy dataset to a file
write.table(allData.mean, "tidy1.txt", row.names = FALSE, quote = FALSE)

tidy_data <- read.table("tidy1.txt", header = TRUE)
head(tidy_data)

