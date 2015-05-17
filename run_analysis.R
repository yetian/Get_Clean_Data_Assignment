# One of the most exciting areas in all of data science right now is wearable computing 
# - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing 
# to develop the most advanced algorithms to attract new users. The data linked to from
# the course website represent data collected from the accelerometers from the Samsung 
# Galaxy S smartphone. A full description is available at the site where the data was obtained: 

# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

# Here are the data for the project: 

# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# The R script called run_analysis.R that does the following:
# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set.
# 4.Appropriately labels the data set with descriptive variable names. 
# 5.From the data set in step 4, creates a second, independent tidy data set
#   with the average of each variable for each activity and each subject.

################
# Implementation
################

####################################################################
### 1. Merges the training and the test sets to create one data set.
####################################################################

trainset_read <- read.table("UCI HAR Dataset/train/subject_train.txt")
testset_read <- read.table("UCI HAR Dataset/test/subject_test.txt")

subjectset <- rbind(trainset_read, testset_read)

trainset_read <- read.table("UCI HAR Dataset/train/X_train.txt")
testset_read <- read.table("UCI HAR Dataset/test/X_test.txt")

xset <- rbind(trainset_read, testset_read)

trainset_read <- read.table("UCI HAR Dataset/train/y_train.txt")
testset_read <- read.table("UCI HAR Dataset/test/y_test.txt")

yset <- rbind(trainset_read, testset_read)

# remove temporary variables
rm(trainset_read)
rm(testset_read)

#############################################################################################
### 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
#############################################################################################
features <- read.table("UCI HAR Dataset/features.txt")

# get ids of columns of mean and standard deviation
idx_mean_sd <- grep("-mean\\(\\)|std\\(\\)", features[,2])

# Extract mean and sd from xset
xset_extract <- xset[, idx_mean_sd]

# extract names from features
cnames <- features[idx_mean_sd, 2]

# remove brackets ()
cnames <- tolower(gsub("\\(\\)", "", cnames))

# name the xset_extract
names(xset_extract) <- cnames

#############################################################################
### 3.Uses descriptive activity names to name the activities in the data set.
#############################################################################
activities <- read.table("UCI HAR Dataset/activity_labels.txt")

# lower the case in activities
activities[,2] <- tolower(activities[,2])

# name activities. yset[,1] returns the value, which equals to the idx of 
# each row in activities.txt. activities[*, 2] then gets the name in lowercase.
yset[,1] = activities[yset[,1], 2]

names(yset) = "activity"

########################################################################
### 4.Appropriately labels the data set with descriptive variable names.
########################################################################
names(subjectset) <- "subject"

# combine columns
cleanedset <- cbind(subjectset, yset, xset_extract)

# test: write out step4.txt
# write.table(cleanedset, "step4.txt")

##############################################################################
### 5.From the data set in step 4, creates a second, independent tidy data set
###   with the average of each variable for each activity and each subject.
##############################################################################

# get unique subjects, column -> row
unique_subjectset <- unique(subjectset)[,1]

# get the number of unique subjects
num_unique_subjects <- length(unique_subjectset)

# get the number of activities, column -> row
num_activities <- length(activities[,1])

# get the number of columns in the cleanedset
num_columns <- dim(cleanedset)[2]

# thus the number of calculations as well as result rows
# should be num_activities * num_unique_subjects.
num_calc <- num_activities * num_unique_subjects

# Average set has the same column structure as cleaned set, thus
# directly create average set from cleaned set.
# Later, update directly the values.
averageset <- cleanedset[1:num_calc, ]

# update

# counter
i <- 0

for (s in 1:num_unique_subjects)
{
  for (a in 1:num_activities)
  {
    # update subjects
    averageset[i, 1] <- unique_subjectset[s]
    
    # update activities, get the name from the activities table.
    activity_name <- activities[a, 2]
    averageset[i, 2] <- activity_name
    
    # calc means for the rest of the columns
    # make subset for subject = s and activities = name_activity[a]
    sub_cleanedset <- cleanedset[cleanedset["subject"] == s & cleanedset["activity"] == activity_name, ] 
    
    # rest of the columns starts from 3 to num_columns
    averageset[i, 3:num_columns] <- colMeans(sub_cleanedset[,3:num_columns])
    
    # move to next row
    i = i + 1
  }
}

write.table(averageset, "step5.txt", row.name=FALSE)



