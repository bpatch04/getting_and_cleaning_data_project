---
title: "README"
Author: 'Brent Patchin'
output: html_document
---

# Markdown

This is an READ ME Markdown document for the Coursera Data Science Specialization, Getting and Data Cleaning, Course Project.

### Activity Table

The first step is to read the activities table. This table shows 6 activities and their activity id, we will name the fields to make it easy to lookup the activity names later on.

```{r}
act_lab <- read.csv('getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt',header=F,sep=" ")
names(act_lab) <- c('activity_id','activity')
```

### Features Table

Then we will read in the features table. This table shows the feature ID and the feature name, however the feature names required some cleanup.
1. There were features that had numbers right after the formula, ie. mean()1, so we add a dash - between the formula and the number
2. There were features that had an extra open bracket, we remove these
3. The angle features were written different, so we formatted this to have the angle name, then the angle formula.
4. The maxInds did not have an open and closed bracket at the end so we add those.
5. We updated the features that start with t to Time
6. We updated the features that start with f to Freq
7. Finally we changed all of the dashes to periods so we can use these as field names in the final tidy dataset

```{r}
features <- read.csv('getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt',header=F,sep=" ")
features$V2 <- sub("[)]([0-9])$",")-\\1",features$V2)
features$V2 <- sub("(.*)[)],(.*)","\\1,\\2",features$V2)
features$V2 <- sub("^angle[(](.*)[)]$","\\1-angle()",features$V2)
features$V2 <- sub('maxInds','maxInds()',features$V2)
features$V2 <- sub('^t(.*)','Time\\1',features$V2)
features$V2 <- sub('^f(.*)','Freq\\1',features$V2)
features$V2 <- gsub('[-]','.',features$V2)
names(features) <- c('feature_id',"feature")
```

### Subjects Table

We bring in the subjects table for both train and test and combine into one subjects table.

```{r, echo=FALSE}
subject_test <- read.csv('getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt')
subject_train <- read.csv('getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt')
names(subject_test) <- c('subject')
names(subject_train) <- c('subject')
subjects <- rbind(subject_train,subject_test)
```


### X Table

We bring in the X table for both train and test and combine into one X table. We also rename the column fields numerically from 1 to 561 so we know based on the features id which field is for which feature.

```{r, echo=FALSE}
x_test <- fread('getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt',header=T,sep=" ")
x_train <- fread('getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt',header=T,sep=" ")
names(x_test) <- as.character(c(1:561))
names(x_train) <- as.character(c(1:561))
x_final <- rbind(x_train,x_test)
```

### Y Table

We bring in the Y table for both train and test and combine into one Y table.
We rename the field as activity_id and bring in the activity name by merging with the activity_id from the activity table.

```{r, echo=FALSE}
y_test <- read.csv('getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt',header=T,sep=" ")
y_train <- read.csv('getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt',header=T,sep=" ")
names(y_train) <- c('activity_id')
names(y_test) <- c('activity_id')
y_train <- merge(y_train,act_lab,by='activity_id',sort=F)
y_test <- merge(y_test,act_lab,by='activity_id',sort=F)
y_final <- rbind(y_train,y_test)
```

### Extracts only the features on the mean and standard deviation. 

We first get the mean features from the features table and select only those columns from X table and update the column names in the X table.
We then get the standard deviations features from the features table and select only those columns from X table and update the column names in the X table.

We split this into 2 X tables, one for mean and one for Standard deviation, so we can properly summarize fields later. We will combine these two tables later

```{r, echo=FALSE}
mf <- grep("mean[()]",features$feature)
mfv <- grep("mean[()]",features$feature,value=T)
x_final_mean <- x_final %>% select(mf)
mfv <- sub('[(]','',mfv)
mfv <- sub('[)]','',mfv)
colnames(x_final_mean) <- mfv

sf <- grep("std[()]",features$feature)
sfv <- grep("std[()]",features$feature,value=T)
x_final_std <- x_final %>% select(sf)
sfv <- sub('[(]','',sfv)
sfv <- sub('[)]','',sfv)
colnames(x_final_std) <- sfv
```

### combine subject, x and y into one table. 

Combine subjects, x table (mean and std) and y table. We keep the mean and standard deviation tables separate still for summarizing.

```{r, echo=FALSE}
syx_mean <- cbind(subjects,y_final,x_final_mean)
syx_mean <- select(syx_mean,-activity_id)
syx_std <- cbind(subjects,y_final,x_final_std)
syx_std <- select(syx_std,-activity_id)
```

### Final tidy dataset

We combine mean and std into one tidy dataset, but will still use the separated ones for the grouping and summary.

```{r, echo=FALSE}
syx_tidy <- cbind(syx_mean,syx_std)
syx_tidy
write.table(syx_tidy,'getdata_projectfiles_UCI HAR Dataset/syx_tidy.txt',row.name=F)
```

### Grouping and Summary

We now group by the subjects and activity and take the mean/standard deviations of all of the other columns. Then we combine these two summary tables into one table and remove the duplicated subject/acivity columns.
Then we pivot the features to be a single column with a value column.

```{r, echo=FALSE}
syx_mean2 <- syx_mean %>% group_by(subject,activity) %>% summarize_all(funs(mean))
syx_std2 <- syx_std %>% group_by(subject,activity) %>% summarize_all(funs(mean))

syx_summarized <- cbind(syx_mean2,syx_std2)
syx_summarized <- select(syx_summarized,-(36:37))
colnames(syx_summarized)[1:2] <- c('subject','activity')
syx_summarized <- pivot_longer(syx_summarized,!(subject:activity), names_to = "features", values_to = "value")
```

### Final summarized tidy dataset

This is the final summarize tidy dataset from part 5.

```{r, echo=FALSE}
syx_summarized
write.table(syx_summarized,'getdata_projectfiles_UCI HAR Dataset/syx_summarized.txt',row.name=F)
```
