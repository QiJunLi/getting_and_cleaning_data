unzip("getdata-projectfiles-UCI HAR Dataset.zip")
##1 loading data
testSet <- read.table("./UCI HAR Dataset/test/X_test.txt",stringsAsFactors = FALSE)
trainSet <- read.table("./UCI HAR Dataset/train/X_train.txt",stringsAsFactors = FALSE)

ytest <- read.table("./UCI HAR Dataset/test/y_test.txt",stringsAsFactors = FALSE)
ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt",stringsAsFactors = FALSE)

testSubject <- read.table("./UCI HAR Dataset/test/subject_test.txt",stringsAsFactors = FALSE)
trainSubject <- read.table("./UCI HAR Dataset/train/subject_train.txt",stringsAsFactors = FALSE)

features <- read.table("./UCI HAR Dataset/features.txt",stringsAsFactors = FALSE)
features <- features$V2
features <- c(features, "actLable","subject")
##2 merging test and train datasets into one dataset
testSet <- cbind(testSet, ytest,testSubject)
trainSet <- cbind(trainSet, ytrain,trainSubject)
names(testSet)[563] <- "subject"
names(trainSet)[563] <- "subject"

dataset <- rbind(testSet,trainSet)
names(dataset) <- features
##3 subset data
subdata <- dataset[,c(1:6, 41:46, 81:86, 121:126, 161:166,201,202,214,215,227,228,240,241,253,254,266:271,345:350,424:429,503,504,516,517,529,530,542,543,563)]
##4 rename the activities
num <- nrow(subdata)
actLable <- dataset$actLable
activity <- character()
for(i in 1:num){
    if (actLable[i] == 1) activity[i]<-"WALKING";
    if (actLable[i] == 2) activity[i]<-"WALKING_UPSTAIRS";
    if (actLable[i] == 3) activity[i]<-"WALKING_DOWNSTAIRS";
    if (actLable[i] == 4) activity[i]<-"SITTING";
    if (actLable[i] == 5) activity[i]<-"STANDING";
    if (actLable[i] == 6) activity[i]<-"LAYING";
}
subdata <- cbind(subdata, activity)    
##5 obtain the mean of each variable grouped by subjects and activities
orderedData <- subdata[order(subdata$subject, subdata$activity, decreasing =FALSE),]
sp <- split(orderedData, orderedData[,c("subject", "activity")])
meanFUN <- function(df){
    dfnew <- data.frame();
    for(i in 1:66) dfnew[1,i] <- mean(df[1,i]);
    subject <- df$subject[1];
    activity <- df$activity[1];
    dfnew <- cbind(dfnew,subject,activity);
    dfnew;
}
result<-lapply(sp,meanFUN)
##6 merge the lists
df <- result[[1]]
for(i in 2:length(result)) df <- rbind(df,result[[i]])
df <- df[order(c(df$subject,df$avtivity),decreasing = FALSE),]
names(df) <- names(orderedData)
##7 write table
write.table(df,"activityFeaturesMean.txt")
