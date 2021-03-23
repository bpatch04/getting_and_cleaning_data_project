library("stringr")
library("tidyr")
library('dplyr')

#Get Activity Table
act_lab <- read.csv('getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt',header=F,sep=" ")
names(act_lab) <- c('activity_id','activity')

#Get Features Table and clean up
features <- read.csv('getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt',header=F,sep=" ")
features$V2 <- sub("[)]([0-9])$",")-\\1",features$V2)
features$V2 <- sub("(.*)[)],(.*)","\\1,\\2",features$V2)
features$V2 <- sub("^angle[(](.*)[)]$","\\1-angle()",features$V2)
features$V2 <- sub('maxInds','maxInds()',features$V2)
features$V2 <- sub('^t(.*)','Time\\1',features$V2)
features$V2 <- sub('^f(.*)','Freq\\1',features$V2)
features$V2 <- gsub('[-]','.',features$V2)
names(features) <- c('feature_id',"feature")

#Get Subjects List for Train and Test and combine them
subject_test <- read.csv('getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt')
subject_train <- read.csv('getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt')
names(subject_test) <- c('subject')
names(subject_train) <- c('subject')
subjects <- rbind(subject_train,subject_test)

#get X table for train and test and combine.
x_test <- fread('getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt',header=T,sep=" ")
x_train <- fread('getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt',header=T,sep=" ")
names(x_test) <- as.character(c(1:561))
names(x_train) <- as.character(c(1:561))
x_final <- rbind(x_train,x_test)

#get Y table for train and test and combine. Lookup activity Id to get activity name from activity table
y_test <- read.csv('getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt',header=T,sep=" ")
y_train <- read.csv('getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt',header=T,sep=" ")
names(y_train) <- c('activity_id')
names(y_test) <- c('activity_id')
y_train <- merge(y_train,act_lab,by='activity_id',sort=F)
y_test <- merge(y_test,act_lab,by='activity_id',sort=F)
y_final <- rbind(y_train,y_test)

#Get only the mean features from x table  and get features name as column names
mf <- grep("mean[()]",features$feature)
mfv <- grep("mean[()]",features$feature,value=T)
x_final_mean <- x_final %>% select(mf)
mfv <- sub('[(]','',mfv)
mfv <- sub('[)]','',mfv)
colnames(x_final_mean) <- mfv

#Get only the std features from x table and get features name as column names
sf <- grep("std[()]",features$feature)
sfv <- grep("std[()]",features$feature,value=T)
x_final_std <- x_final %>% select(sf)
sfv <- sub('[(]','',sfv)
sfv <- sub('[)]','',sfv)
colnames(x_final_std) <- sfv

#Combine subjects, x table (mean and std) and y table
syx_mean <- cbind(subjects,y_final,x_final_mean)
syx_mean <- select(syx_mean,-activity_id)
syx_std <- cbind(subjects,y_final,x_final_std)
syx_std <- select(syx_std,-activity_id)

#this is the final tidy dataset
syx_tidy <- cbind(syx_mean,syx_std)
syx_tidy
write.table(syx_tidy,'getdata_projectfiles_UCI HAR Dataset/syx_tidy.txt',row.name=F)


#Group by subjects and activities and calculate mean featues and std features
syx_mean2 <- syx_mean %>% group_by(subject,activity) %>% summarize_all(funs(mean))
syx_std2 <- syx_std %>% group_by(subject,activity) %>% summarize_all(funs(mean))

#combine the mean features and std features into one table
syx_summarized <- cbind(syx_mean2,syx_std2)
syx_summarized <- select(syx_summarized,-(36:37))
colnames(syx_summarized)[1:2] <- c('subject','activity')
syx_summarized <- pivot_longer(syx_summarized,!(subject:activity), names_to = "features", values_to = "value")

#this is the final tidy dataset that is summarized
syx_summarized

write.table(syx_summarized,'getdata_projectfiles_UCI HAR Dataset/syx_summarized.txt',row.name=F)