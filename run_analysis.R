#Erik Oberg 11/20/2015
#requires dplyr
library(dplyr)

###Set the UCI HAR dataset folder as your working directory!

# 1. Merges the training and the test sets to create one data set.
    #First read what the variables are and store in table for renaming columns
    Features<-read.table("features.txt")
    
    #Get list of subjects and rename column Subject
    Subject<-read.table("test/subject_test.txt")
    colnames(Subject) <- "Subject"
    #Read through test files rename X table headers with the features column so that names are DESCRIPTIVE!
    X<-read.table("test/X_test.txt")
# 4.  Appropriately labels the data set with descriptive variable names. (did item 4 now in order to use throughout script)
    colnames(X) <- Features$V2
    # Activity labels will remain numeric for now, but add descriptive header "Activity_Label
    Y<-read.table("test/y_test.txt")
    colnames(Y) <- "Activity_Label"
    #Add a column to indicate this is "Test" data. This will is in case you ever want to separate test and train data after combining.
    original_table<-rep("Test",length(Subject$Subject))
    #put all test data together in table
    test<-data.frame(original_table,Subject,X,Y)
    
    
    #Now repeat above process for train data
    Subject<-read.table("train/subject_train.txt")
    colnames(Subject) <- "Subject"
    X<-read.table("train/X_train.txt")
    colnames(X) <- Features$V2
    Y<-read.table("train/y_train.txt")
    colnames(Y) <- "Activity_Label"
    original_table<-rep("Train",length(Subject$Subject))
    train<-data.frame(original_table,Subject,X,Y)
    
    #since all column headers are the same we can 'stack' the two datasets on top of each other using rbind 
    test_train <- rbind(test, train)
    
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
    
    #Now lets only select the columns that are mean or standard deviations. Do separately then combine columns
    means<-select(test_train, contains("mean"))
    std<-select(test_train, contains("std"))
    
    tidydata<-cbind(test_train$Subject,test_train$Activity_Label,means,std)
    #In the process adding tables together column names have changed; rename
    colnames(tidydata)<-c("Subject",'Activity_Label',colnames(means),colnames(std))
    
# 3. Uses descriptive activity names to name the activities in the data set
      
    #Get activity descriptions and replace numeric Activity_Label
    Activity_Label_dictionary<-read.table("activity_labels.txt")
    colnames(Activity_Label_dictionary)<-c("Activity_Label","Activity_description")
    tidydata<-left_join(tidydata,Activity_Label_dictionary, by="Activity_Label")
    #Now we have Activity Description, which is a DESCRIPTIVE label for that activity!!! Much easier to remember what it is and makes more sense grouping as it is a factor.
    #Drop the numeric Activity_Label since we have another column representing that data
    tidydata<-select(tidydata, -Activity_Label)

# 5. Finally summarise. "A second, independent tidy data set with the average of each variable for each activity and each subject."
    tidydatasummary<-tidydata %>% group_by(Subject, Activity_description) %>% summarise_each(funs(mean))
    
write.table(tidydatasummary,file="EOberg_projcet2_table.txt",row.names=FALSE)