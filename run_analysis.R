library(dplyr)
dir_ <- "./UCI HAR Dataset/"
labels_ <- read.table(paste(dir_, "activity_labels.txt", sep=""), stringsAsFactors=FALSE)
feat_ <- read.table(paste(dir_, "features.txt", sep=""), stringsAsFactors=FALSE)
train_test <- list(train="train", test="test")
# The next lines should read 
tables_ <- list(X="X_", subject="subject_", y="y_")
frames_ <- c(feat_[,2], "subj", "activ")
# The shorter versions are used for testing the program without loading the huge database
# tables_ <- list(subject="subject_", y="y_")
# frames_ <- c("subj", "activ")
new_function <- function(x,w) {
    table_name <- paste(dir_, x, "/", w, x, ".txt", sep="")
    read.table(table_name)
}
good_function <- function(w) {
    z <- lapply(train_test,new_function,w)
    names(z) <- c("train", "test")
    rbind(z$train,z$test)
}
ColsSubjY <- lapply(tables_, good_function)
Big_Data <- cbind(ColsSubjY[1]$X, ColsSubjY[2]$subject, ColsSubjY[3]$y)
# Big_Data <- cbind(ColsSubjY[1]$subject, ColsSubjY[2]$y)
names(Big_Data) <- frames_
Extract_ <- Big_Data[,(grepl("mean()",names(Big_Data), fixed=TRUE) | grepl("std()",names(Big_Data), fixed=TRUE))]
names(Extract_) <- tolower(names(Extract_))
names(Extract_) <- sub("tb","timeb", names(Extract_))
names(Extract_) <- sub("tg","timeg", names(Extract_))
names(Extract_) <- sub("f","frequency", names(Extract_))
names(Extract_) <- gsub("-", "", names(Extract_))
names(Extract_) <- gsub("\\()", "", names(Extract_))
names(Extract_) <- sub("acc", "accelerometer", names(Extract_))
names(Extract_) <- sub("gyro", "gyroscope", names(Extract_))
names(Extract_) <- sub("mag", "magnitude", names(Extract_))
names(Extract_) <- sub("bodybody", "body", names(Extract_))
names(Extract_) <- sub("std", "standarddeviation", names(Extract_))
Extract_$subject <- Big_Data$subj
Extract_$activity <- labels_[Big_Data$activ, 2]
print(dim(Extract_))
Tidy_ <- ddply(Extract_, .(subject,activity), numcolwise(mean))
print(dim(Tidy_))
print(head(Tidy_))
write.table(Tidy_, "Tidy_Dataset.txt", row.names=FALSE)