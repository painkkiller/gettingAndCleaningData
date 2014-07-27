##
##    Program Description:
##    This program Merges the training and the test sets to create one data set. It re-labels 
##    all the columns to match the corresponding features and activities.
##
##    It extracts only the measurements on the mean and standard deviation for each measurement 
##    and then exports a tidy dataset to a file called "tidyData.csv"
##
##    A second, independent tidy data set with the average of each variable for each activity 
##    and each subject is created an exported as "averages.csv". 
##
##

## Load Libraries
library(sqldf)
library(data.table)

################################################################################################
##    Read in reference data from files and add column names
################################################################################################
features <- data.frame(read.table("./UCI HAR Dataset/features.txt", header=FALSE))
activities <- data.frame(read.table("./UCI HAR Dataset/activity_labels.txt", header=FALSE))

names(features) <- c("id","name")
names(activities) <- c("id","activityDesc")

################################################################################################
##    Read in Test data from files and add column names
################################################################################################
ttS <- data.frame(read.table("./UCI HAR Dataset/test/subject_test.txt", header=FALSE))
ttX <- data.frame(read.table("./UCI HAR Dataset/test/X_test.txt", header=FALSE, colClasses="numeric"))
ttY <- data.frame(read.table("./UCI HAR Dataset/test/Y_test.txt", header=FALSE, colClasses="numeric"))

names(ttS) <- "Subjects"
names(ttX) <- features$name
names(ttY) <- "activityCodes"

## Join ttY to activity descriptions
actDesc <- sqldf('select b.activityDesc from ttY a left join activities b on a.activityCodes = b.id')

## Add an id column to ttX for joining
ttX["id"] <- 1:nrow(ttX)

################################################################################################
##    Make a huge flat data frame for Test Data called 'tt'
################################################################################################

tt <- data.frame(1:nrow(ttS))
names(tt) <- "id"
tt["Subjects"] <- ttS
tt["activityDesc"] <- actDesc
tt <- merge(tt, ttX, by.x="id", by.y="id")
tt["dataSetInd"] <- "TEST"
## remove any data frames you dont need to save memory
rm(ttS, ttX, ttY,actDesc)


################################################################################################
##    Read in Training data from files and add column names
################################################################################################
tnS <- data.frame(read.table("./UCI HAR Dataset/train/subject_train.txt", header=FALSE))
tnX <- data.frame(read.table("./UCI HAR Dataset/train/X_train.txt", header=FALSE, colClasses="numeric"))
tnY <- data.frame(read.table("./UCI HAR Dataset/train/Y_train.txt", header=FALSE, colClasses="numeric"))

names(tnS) <- "Subjects"
names(tnX) <- features$name
names(tnY) <- "activityCodes"

## Join tnY to activity descriptions
actDesc <- sqldf('select b.activityDesc from tnY a left join activities b on a.activityCodes = b.id')

## Add an id column to tnX for joining
tnX["id"] <- 1:nrow(tnX)

################################################################################################
##    Make a huge flat data frame for Training Data called 'tn'
################################################################################################

tn <- data.frame(1:nrow(tnS))
names(tn) <- "id"
tn["Subjects"] <- tnS
tn["activityDesc"] <- actDesc
tn <- merge(tn, tnX, by.x="id", by.y="id")
tn["dataSetInd"] <- "TRAINING"
## remove any data frames you dont need to save memory
rm(tnS, tnX, tnY,actDesc)

#########################################################################################################################
##    Merge training and test data sets (dataSetInd describes which dataset the record came from e.g. Training or Test)
#########################################################################################################################

data <- rbind(tn, tt)

#########################################################################################################################
##    Get only measurements on the mean and standard deviation for each measurement
#########################################################################################################################
## Use Grep to get only column names with std() and mean() in them and return the column numbers (or names?)
mypattern <- c("std","mean")
names <- names(data)

## Check each column name for std or mean
mycols <- character()
for (s in mypattern) {
  mycolmask <- lapply(names, grep, pattern=s, ignore.case = TRUE )
  mycols <- cbind(mycols, names[mycolmask == TRUE])
}

colStd <- data.frame(mycols[!is.na(mycols[,1]),1])
names(colStd) <- "columns"
colMean<- data.frame(mycols[!is.na(mycols[,2]),2])
names(colMean) <- "columns"

getCols <- rbind(colStd, colMean)

#########################################################################################################################
##    Create Tidy Dataset
#########################################################################################################################
## getCols contains the required cols for our tidy dataset. List them here to select them from our data frame called data
tidyData <- subset(data, select=c("id","Subjects","activityDesc","tBodyAcc-std()-X", "tBodyAcc-std()-Y", "tBodyAcc-std()-Z", "tGravityAcc-std()-X", "tGravityAcc-std()-Y", "tGravityAcc-std()-Z", "tBodyAccJerk-std()-X", "tBodyAccJerk-std()-Y", "tBodyAccJerk-std()-Z", "tBodyGyro-std()-X", "tBodyGyro-std()-Y", "tBodyGyro-std()-Z", "tBodyGyroJerk-std()-X", "tBodyGyroJerk-std()-Y", "tBodyGyroJerk-std()-Z", "tBodyAccMag-std()", "tGravityAccMag-std()", "tBodyAccJerkMag-std()", "tBodyGyroMag-std()", "tBodyGyroJerkMag-std()", "fBodyAcc-std()-X", "fBodyAcc-std()-Y", "fBodyAcc-std()-Z", "fBodyAccJerk-std()-X", "fBodyAccJerk-std()-Y", "fBodyAccJerk-std()-Z", "fBodyGyro-std()-X", "fBodyGyro-std()-Y", "fBodyGyro-std()-Z", "fBodyAccMag-std()", "fBodyBodyAccJerkMag-std()", "fBodyBodyGyroMag-std()", "fBodyBodyGyroJerkMag-std()", "tBodyAcc-mean()-X", "tBodyAcc-mean()-Y", "tBodyAcc-mean()-Z", "tGravityAcc-mean()-X", "tGravityAcc-mean()-Y", "tGravityAcc-mean()-Z", "tBodyAccJerk-mean()-X", "tBodyAccJerk-mean()-Y", "tBodyAccJerk-mean()-Z", "tBodyGyro-mean()-X", "tBodyGyro-mean()-Y", "tBodyGyro-mean()-Z", "tBodyGyroJerk-mean()-X", "tBodyGyroJerk-mean()-Y", "tBodyGyroJerk-mean()-Z", "tBodyAccMag-mean()", "tGravityAccMag-mean()", "tBodyAccJerkMag-mean()", "tBodyGyroMag-mean()", "tBodyGyroJerkMag-mean()", "fBodyAcc-mean()-X", "fBodyAcc-mean()-Y", "fBodyAcc-mean()-Z", "fBodyAcc-meanFreq()-X", "fBodyAcc-meanFreq()-Y", "fBodyAcc-meanFreq()-Z", "fBodyAccJerk-mean()-X", "fBodyAccJerk-mean()-Y", "fBodyAccJerk-mean()-Z", "fBodyAccJerk-meanFreq()-X", "fBodyAccJerk-meanFreq()-Y", "fBodyAccJerk-meanFreq()-Z", "fBodyGyro-mean()-X", "fBodyGyro-mean()-Y", "fBodyGyro-mean()-Z", "fBodyGyro-meanFreq()-X", "fBodyGyro-meanFreq()-Y", "fBodyGyro-meanFreq()-Z", "fBodyAccMag-mean()", "fBodyAccMag-meanFreq()", "fBodyBodyAccJerkMag-mean()", "fBodyBodyAccJerkMag-meanFreq()", "fBodyBodyGyroMag-mean()", "fBodyBodyGyroMag-meanFreq()", "fBodyBodyGyroJerkMag-mean()", "fBodyBodyGyroJerkMag-meanFreq()", "angle(tBodyAccMean,gravity)", "angle(tBodyAccJerkMean),gravityMean)", "angle(tBodyGyroMean,gravityMean)", "angle(tBodyGyroJerkMean,gravityMean)", "angle(X,gravityMean)", "angle(Y,gravityMean)", "angle(Z,gravityMean)","dataSetInd" ))

write.csv(tidyData, file="tidyData.csv", row.names=FALSE)

#########################################################################################################################
##    Create second, independant dataset with averages of each variable for each activity 
##    and each subject. Export as "averages.csv".
#########################################################################################################################
## Create data.table called dt (data table is faster for aggregations and merges)
dt <- as.data.table(tidyData)
dt[, list(Mean = colMeans( dt[,list( tBodyAccstdX)] )), by=list(activityDesc,Subjects)]

## Clear up some memory
rm(tidyData, activities, colMean, colStd, data, features, getCols, mycols, tn, tt)

## Remove '-' and '()' from names in data table so they can be accessed
names(dt) <- gsub("[-|(|)|,]", "", names(dt), ignore.case=TRUE)

## Group by Activities and Subjects and calculate the mean for each column of mean and standard deviation measurements
a1 <- sqldf("select activityDesc, Subjects, avg(tBodyAccstdX) from dt where tBodyAccstdX is not null group by activityDesc, Subjects;")
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyAccstdY) from dt where tBodyAccstdY is not null group by activityDesc, Subjects;"))
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyAccstdZ) from dt where tBodyAccstdZ is not null group by activityDesc, Subjects;"))
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tGravityAccstdX) from dt where tGravityAccstdX is not null group by activityDesc, Subjects;"))
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tGravityAccstdY) from dt where tGravityAccstdY is not null group by activityDesc, Subjects;"))
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tGravityAccstdZ) from dt where tGravityAccstdZ is not null group by activityDesc, Subjects;"))
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyAccJerkstdX) from dt where tBodyAccJerkstdX is not null group by activityDesc, Subjects;"))
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyAccJerkstdY) from dt where tBodyAccJerkstdY is not null group by activityDesc, Subjects;"))
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyAccJerkstdZ) from dt where tBodyAccJerkstdZ is not null group by activityDesc, Subjects;"))
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyGyrostdX) from dt where tBodyGyrostdX is not null group by activityDesc, Subjects;"))
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyGyrostdY) from dt where tBodyGyrostdY is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyGyrostdZ) from dt where tBodyGyrostdZ is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyGyroJerkstdX) from dt where tBodyGyroJerkstdX is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyGyroJerkstdY) from dt where tBodyGyroJerkstdY is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyGyroJerkstdZ) from dt where tBodyGyroJerkstdZ is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyAccMagstd) from dt where tBodyAccMagstd is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tGravityAccMagstd) from dt where tGravityAccMagstd is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyAccJerkMagstd) from dt where tBodyAccJerkMagstd is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyGyroMagstd) from dt where tBodyGyroMagstd is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyGyroJerkMagstd) from dt where tBodyGyroJerkMagstd is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyAccstdX) from dt where fBodyAccstdX is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyAccstdY) from dt where fBodyAccstdY is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyAccstdZ) from dt where fBodyAccstdZ is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyAccJerkstdX) from dt where fBodyAccJerkstdX is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyAccJerkstdY) from dt where fBodyAccJerkstdY is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyAccJerkstdZ) from dt where fBodyAccJerkstdZ is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyGyrostdX) from dt where fBodyGyrostdX is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyGyrostdY) from dt where fBodyGyrostdY is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyGyrostdZ) from dt where fBodyGyrostdZ is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyAccMagstd) from dt where fBodyAccMagstd is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyBodyAccJerkMagstd) from dt where fBodyBodyAccJerkMagstd is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyBodyGyroMagstd) from dt where fBodyBodyGyroMagstd is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyBodyGyroJerkMagstd) from dt where fBodyBodyGyroJerkMagstd is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyAccmeanX) from dt where tBodyAccmeanX is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyAccmeanY) from dt where tBodyAccmeanY is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyAccmeanZ) from dt where tBodyAccmeanZ is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tGravityAccmeanX) from dt where tGravityAccmeanX is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tGravityAccmeanY) from dt where tGravityAccmeanY is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tGravityAccmeanZ) from dt where tGravityAccmeanZ is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyAccJerkmeanX) from dt where tBodyAccJerkmeanX is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyAccJerkmeanY) from dt where tBodyAccJerkmeanY is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyAccJerkmeanZ) from dt where tBodyAccJerkmeanZ is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyGyromeanX) from dt where tBodyGyromeanX is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyGyromeanY) from dt where tBodyGyromeanY is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyGyromeanZ) from dt where tBodyGyromeanZ is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyGyroJerkmeanX) from dt where tBodyGyroJerkmeanX is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyGyroJerkmeanY) from dt where tBodyGyroJerkmeanY is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyGyroJerkmeanZ) from dt where tBodyGyroJerkmeanZ is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyAccMagmean) from dt where tBodyAccMagmean is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tGravityAccMagmean) from dt where tGravityAccMagmean is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyAccJerkMagmean) from dt where tBodyAccJerkMagmean is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyGyroMagmean) from dt where tBodyGyroMagmean is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(tBodyGyroJerkMagmean) from dt where tBodyGyroJerkMagmean is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyAccmeanX) from dt where fBodyAccmeanX is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyAccmeanY) from dt where fBodyAccmeanY is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyAccmeanZ) from dt where fBodyAccmeanZ is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyAccmeanFreqX) from dt where fBodyAccmeanFreqX is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyAccmeanFreqY) from dt where fBodyAccmeanFreqY is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyAccmeanFreqZ) from dt where fBodyAccmeanFreqZ is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyAccJerkmeanX) from dt where fBodyAccJerkmeanX is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyAccJerkmeanY) from dt where fBodyAccJerkmeanY is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyAccJerkmeanZ) from dt where fBodyAccJerkmeanZ is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyAccJerkmeanFreqX) from dt where fBodyAccJerkmeanFreqX is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyAccJerkmeanFreqY) from dt where fBodyAccJerkmeanFreqY is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyAccJerkmeanFreqZ) from dt where fBodyAccJerkmeanFreqZ is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyGyromeanX) from dt where fBodyGyromeanX is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyGyromeanY) from dt where fBodyGyromeanY is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyGyromeanZ) from dt where fBodyGyromeanZ is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyGyromeanFreqX) from dt where fBodyGyromeanFreqX is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyGyromeanFreqY) from dt where fBodyGyromeanFreqY is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyGyromeanFreqZ) from dt where fBodyGyromeanFreqZ is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyAccMagmean) from dt where fBodyAccMagmean is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyAccMagmeanFreq) from dt where fBodyAccMagmeanFreq is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyBodyAccJerkMagmean) from dt where fBodyBodyAccJerkMagmean is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyBodyAccJerkMagmeanFreq) from dt where fBodyBodyAccJerkMagmeanFreq is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyBodyGyroMagmean) from dt where fBodyBodyGyroMagmean is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyBodyGyroMagmeanFreq) from dt where fBodyBodyGyroMagmeanFreq is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyBodyGyroJerkMagmean) from dt where fBodyBodyGyroJerkMagmean is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(fBodyBodyGyroJerkMagmeanFreq) from dt where fBodyBodyGyroJerkMagmeanFreq is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(angletBodyAccMeangravity) from dt where angletBodyAccMeangravity is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(angletBodyAccJerkMeangravityMean) from dt where angletBodyAccJerkMeangravityMean is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(angletBodyGyroMeangravityMean) from dt where angletBodyGyroMeangravityMean is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(angletBodyGyroJerkMeangravityMean) from dt where angletBodyGyroJerkMeangravityMean is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(angleXgravityMean) from dt where angleXgravityMean is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(angleYgravityMean) from dt where angleYgravityMean is not null group by activityDesc, Subjects;")) 
a1 <- merge(a1, sqldf("select activityDesc, Subjects, avg(angleZgravityMean) from dt where angleZgravityMean is not null group by activityDesc, Subjects;")) 

o <- data.table(a1)

## Clear up some memory
rm(a1)

## Order data by Activity then by Subjects
ordered <- o[order(activityDesc, Subjects),  ]

## Clear up some memory
rm(o)

## Write data to file averages.csv in your working directory
write.csv( ordered ,file="averages.csv", row.names=FALSE)