## Set file paths
fileTestX <- "UCI HAR Dataset/test/X_test.txt"
fileTestY <- "UCI HAR Dataset/test/y_test.txt"
fileTestSubject <- "UCI HAR Dataset/test/subject_test.txt"
fileTrainX <- "UCI HAR Dataset/train/X_train.txt"
fileTrainY <- "UCI HAR Dataset/train/y_train.txt"
fileTrainSubject <- "UCI HAR Dataset/train/subject_train.txt"
fileActivityLabels <- "UCI HAR Dataset/activity_labels.txt"
fileColumnNames <- "UCI HAR Dataset/features.txt"

## Read files
dataTestX <- read.table(fileTestX)
dataTestY <- read.table(fileTestY)
dataTestSubject <- read.table(fileTestSubject)

dataTrainX <- read.table(fileTrainX)
dataTrainY <- read.table(fileTrainY)
dataTrainSubject <- read.table(fileTrainSubject)

activities <- read.table(fileActivityLabels)
activities <- activities[[2]]

dataColumnNames <- read.table(fileColumnNames)

## Insert activities and subjects as first and second columns in data[Test & Train]X
dataTestX <- cbind(dataTestY, dataTestSubject, dataTestX)
dataTrainX <- cbind(dataTrainY, dataTrainSubject, dataTrainX)

## Merge measurements test and train into data
data <- rbind(dataTrainX, dataTestX)

## Processing dataColumnNames to remove invalid character
dataColumnNames <- as.character(dataColumnNames[[2]])
dataColumnNames <- gsub("\\()", "", dataColumnNames)
dataColumnNames <- gsub("\\(", "", dataColumnNames)
dataColumnNames <- gsub("\\)", "", dataColumnNames)
dataColumnNames <- gsub(",", "-", dataColumnNames)

## Lowercased all variables name
dataColumnNames <- tolower(dataColumnNames)

## Set column names to dfs
colnames(data) <- c("activities", "subject", dataColumnNames)

## Substitute activities id with activities factor
data$activities <- activities[data$activities]

## Get all columns that contains "mean" or "std" in the name
meanstdColumns <- colnames(data)[grepl("std", names(data)) |
                                 grepl("mean", names(data))]

## Keep all columns with these result and get rid of other columns
data <- data[c("activities", "subject", meanstdColumns)]

## Create tidy data with means for each activity and subject
dataMelt <- melt(data, id.vars=c("activities", "subject"))
meanData <- dcast(dataMelt, activities + subject ~ variable, mean, drop=FALSE)

## Write tidy data to file
write.table(data, "data.txt", row.names=FALSE)
write.table(meanData, "mean_data.txt", row.names=FALSE)
