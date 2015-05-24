##setwd("C:/Users/xiqian/Dropbox/R/coursera/Get Clean Data/UCI HAR Dataset")


##____________________________________________________________
## read the raw data into R
## This part will read the raw data from UCI HAR Dataset into R
## for later analysis 
label.features <- read.table("features.txt")
label.activities <- read.table("activity_labels.txt")
x.test <- read.table("./test/X_test.txt")
y.test <- read.table("./test/y_test.txt")
x.train <- read.table("./train/X_train.txt")
y.train <- read.table("./train/y_train.txt")
subject.test <- read.table("./test/subject_test.txt")
subject.train <- read.table("./train/subject_train.txt")

##______________________________________________________________
## merge the data together
## merge the  test and train data together
## add labels to the column of the data
## add testing features to the row of the data
## add user ID to the row of the data
x.dat <- rbind(x.test, x.train)
y.dat <- rbind(y.test, y.train)
names(y.dat) <- "ExperimentalActivities"
subject.ID <- rbind(subject.test, subject.train)
names(subject.ID) <- c("Subject ID")
names(x.dat) <- label.features[,2]
dat.all <- cbind(y.dat, subject.ID, x.dat)

##________________________________________
## extract the data
## extract a new data set with only two major features
## the standard diviation with the column name as "XXX"-std()
## the mean with the column name as "XXX"-mean()
index.std <- grep("-std()",label.features[,2],fixed=TRUE)
index.mean <- grep("-mean()",label.features[,2],fixed=TRUE)
index.std <- index.std + 2
index.mean <- index.mean + 2
dat.anal <- dat.all[,c(1,2,index.std,index.mean)]
dat.anal <- dat.anal[order(dat.anal$"ExperimentalActivities",
                           dat.anal$"Subject ID"),]

##____________________________________________
## Create a second tidy data
## First isolate each of the experimental activities.
## Then take the average of each of the experimental
## activities of categoried by each of the subject
## ID. 
##
## The "dat.processed" is the required tidy data
ID.activity <- unique(dat.anal$"ExperimentalActivities")
ID.subject <- unique(dat.anal$"Subject ID")
label.activities <- as.character(label.activities[,2])
dat.processed <- data.frame()
for (i in ID.activity){
    for (j in ID.subject){
        dat.sub <- dat.anal[which((dat.anal$"ExperimentalActivities"==i)&
                                      (dat.anal$"Subject ID"==j)),]
        dat.sub <- lapply(dat.sub[,c(-1,-2)], mean)
        dat.sub <- data.frame(label.activities[i],j,dat.sub)
        dat.processed <- rbind(dat.processed, dat.sub)
    }
}

names(dat.processed)[1:2] <- c("ExperimentalActivities","Subject.ID")
