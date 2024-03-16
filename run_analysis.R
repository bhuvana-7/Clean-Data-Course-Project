library(plyr)

# Step 1
# Merge the training and test sets to create one data set
###############################################################################

x_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")
subject_train <- read.table("train/subject_train.txt")

x_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")
subject_test <- read.table("test/subject_test.txt")

# create 'x' data set
x_data <- rbind(x_train, x_test)

# create 'y' data set
y_data <- rbind(y_train, y_test)

# create 'subject' data set
subject_data <- rbind(subject_train, subject_test)

colnames(x_data) <- features[, 2]
colnames(y_data) <- "activitycd"
colnames(subject_data) <- "subjectcd"

#Merge all data set into one data set
all_data <- cbind(x_data, y_data, subject_data)

# Step 2
# Extract only the measurements on the mean and standard deviation for each measurement

mean_std_measure <- grep("activitycd|subjectcd|mean\\(\\)|std\\(\\)", colnames(all_data))
mean_Std_data <- all_data[, mean_std_measure]

# 3. Use descriptive activity names
# Reading activity labels
activity_labels <- read.table("./activity_labels.txt")
colnames(activity_labels) <- c("activitycd", "activity")
mean_Std_data_act_lbls <- merge(mean_Std_data, activity_labels, by = "activitycd", all.x = TRUE)

# 4. Label the data set with descriptive variable names
colnames(mean_Std_data_act_lbls) <- gsub("^t", "time", colnames(mean_Std_data_act_lbls))
colnames(mean_Std_data_act_lbls) <- gsub("^f", "frequency", colnames(mean_Std_data_act_lbls))
colnames(mean_Std_data_act_lbls) <- gsub("Acc", "Accelerometer", colnames(mean_Std_data_act_lbls))
colnames(mean_Std_data_act_lbls) <- gsub("Gyro", "Gyroscope", colnames(mean_Std_data_act_lbls))
colnames(mean_Std_data_act_lbls) <- gsub("Mag", "Magnitude", colnames(mean_Std_data_act_lbls))
colnames(mean_Std_data_act_lbls) <- gsub("BodyBody", "Body", colnames(mean_Std_data_act_lbls))

# 5. Creating a second, independent tidy data set with the avg of each variable for each activity and subject
newtidySet <- mean_Std_data_act_lbls %>%
  group_by(subjectcd, activitycd, activity) %>%
  summarise_all(mean)

# Writing second tidy data set into a txt file
write.table(tidySet, "tidySet.txt", row.names = FALSE)# get only columns with mean() or std() in their names
