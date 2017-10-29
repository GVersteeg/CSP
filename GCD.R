## Script for getting and cleaning the text data for capstone project
## Author: Gerrit Versteeg
## Last saved: 29-10-2017
##
##-----------------------------------------------------------------------------
##---------------- PART 1. GETTING THE DATA -----------------------------------
##
##---- step 0. Loading relevant packages
library("tm")
library("dplyr")
library("magrittr")
##
##---- step 1: prepare the corpus and inspect -----------------------
myLoc <- "./test"
docs <- Corpus(DirSource(myLoc))
summary(docs)
inspect(docs[1])
viewDocs <- function(d,n) {d %>% extract2(n) %>% as.character() %>% writeLines()}
viewDocs(docs, 1)
## ---- 1) getting rid of / @ and | symbols 
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/|@\\|")
## ---- 2) all to lower case
docs <- tm_map(docs, content_transformer(tolower))
## ---- 3) remove numbers
docs <- tm_map(docs, removeNumbers)
## ---- 4) remove all punctuation
docs <- tm_map(docs, removePunctuation)
## ---- 5 get rid of English Stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
## ---- 6 get rid of profanity. Here I used a list by Google, downloaded from:
## https://www.freewebheaders.com/full-list-of-bad-words-banned-by-google/
## as a txt.file. It contains 550 profane words.
## The list contains \xa0 values though. So we need to get rid of those...
BadWords <- readLines("./test/full-list-of-bad-words-banned-by-google-txt-file_2013_11_26_04_53_31_867.txt")
BadWords <- gsub("\xa0", "", BadWords)
docs <- tm_map(docs, removeWords, BadWords)
## ---- 7 remove whitespace
docs <- tm_map(docs, stripWhitespace)
##
##---- step 2: download the zip-file, unzip it and delete it------------------- 
download.file(myURL, destfile = "./data/temp.zip", mode="wb")
dateDownloaded <- date()
unzip("./data/temp.zip", exdir = "./data")
unlink("./data/temp.zip")
##
##-----------------------------------------------------------------------------
##----- PART 2. MAKING DATAFRAMES OF THE RELEVANT DATA ------------------------
##
##---- step 3: read all relevant txt.files into tibbles for speed in dplyr ----
actLabels <- tbl_df(read.table("./data/UCI HAR Dataset/activity_labels.txt", header = FALSE, colClasses = "character"))
features <- tbl_df(read.table("./data/UCI HAR Dataset/features.txt", header = FALSE, colClasses = "character"))
trainSet <- tbl_df(read.table("./data/UCI HAR Dataset/train/X_train.txt", header = FALSE))
testSet <- tbl_df(read.table("./data/UCI HAR Dataset/test/X_test.txt", header = FALSE))
trainSubjects <- tbl_df(read.table("./data/UCI HAR Dataset/train/subject_train.txt", header = FALSE))
testSubjects <- tbl_df(read.table("./data/UCI HAR Dataset/test/subject_test.txt", header = FALSE))
trainActs <- tbl_df(read.table("./data/UCI HAR Dataset/train/y_train.txt", header = FALSE))
testActs <- tbl_df(read.table("./data/UCI HAR Dataset/test/y_test.txt", header = FALSE))
##
##
##-----------------------------------------------------------------------------
##----- PART 3. MERGING THE VARIOUS COMPONENTS INTO A SINGLE DATA FRAME -------
##
##---- step 4: Prepare valid, descriptive column names for measurement variables
nameVector = make.names(features$V2, unique = TRUE, allow_ = TRUE)
nameVector <- sub("\\.\\.","", nameVector)
nameVector <- sub("\\.$","", nameVector)
##
##---- step 5: Name the columns of the data from the test and train group -----
colnames(testActs) <- "Activity"
colnames(testSubjects) <- "Subject"
colnames(testSet) <- nameVector
colnames(trainActs) <- "Activity"
colnames(trainSubjects) <- "Subject"
colnames(trainSet) <- nameVector
##
##---- step 6: Combine the columns of the data from the test-/train groups ----
complete_testSet <- bind_cols(testActs, testSubjects, testSet)
complete_trainSet <- bind_cols(trainActs, trainSubjects, trainSet)
##
##---- step 7: Concatenate the main data from test and train group ------------
completeSet <- bind_rows(complete_trainSet, complete_testSet)
##
##---- step 8: Select Activity, Subject and all column containing "mean" or "std"
subSet <- select(completeSet, Activity, Subject, matches("mean|std"))
##
##---- step 9: Renaming the activities according to the actLabels given -------
subSet$Activity <- plyr::mapvalues(as.factor(subSet$Activity), actLabels$V1, actLabels$V2)
##
##---- cleanup work directory -------------------------------------------------
rm("features")
rm("testActs", "testSubjects", "testSet")
rm("trainActs", "trainSubjects", "trainSet")
rm("complete_trainSet", "complete_testSet")
rm("completeSet")
rm("actLabels")
##
##
##-----------------------------------------------------------------------------
##----- PART 4. CREATING AND WRITING THE TIDY DATASET -------------------------
##
##---- step 10: Sort the resulting subSet on activity and then subject --------
subSetSorted <- arrange(subSet, Activity, Subject)
varNames <- names(subSetSorted)[-c(1, 2)]
##
##---- step 11: Melt the subSet into LONG form --------------------------------
subSetMelt <- melt(subSetSorted, id=c("Activity", "Subject"), measure.vars=varNames)
##
##---- step 12: Use dcast to create a table in WIDE form with avarages --------
tidySet <- dcast(subSetMelt, Activity + Subject ~ variable, mean)
##
##---- step 13: Write text file "tidySet.txt" in the working directory --------
if (file.exists("./tidySet.txt")) {
        unlink("./tidySet.txt")
}
write.table(tidySet, file = "./tidySet.txt", row.names = FALSE)
##
##---- cleanup work directory -------------------------------------------------
rm("subSet", "subSetSorted", "subSetMelt", "tidySet", "varNames", "nameVector")
##
##
##---- message to console upon completion -------------------------------------
message("** Script finished succesfully (find tidySet.txt in your working directory)")
##
##
##-----------------------------------------------------------------------------
## End of script
##-----------------------------------------------------------------------------

