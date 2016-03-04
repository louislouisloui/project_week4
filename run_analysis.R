extract_test <- function() {
  
  require (dplyr)
  
  #Create the data frames
  tbl_y_test <- tbl_df(read.table(file = "./data/UCI HAR Dataset/test/y_test.txt"))
  tbl_subject_test <- tbl_df(read.table(file = "./data/UCI HAR Dataset/test/subject_test.txt"))
  tbl_x_test <- tbl_df(read.table(file = "./data/UCI HAR Dataset/test/X_test.txt"))
  tbl_features <- tbl_df(read.table(file = "./data/UCI HAR Dataset/features.txt"))
  
  #name the tables
  names(tbl_y_test) <- "ActivityNumber"
  names(tbl_subject_test) <- "SubjectNumber"
    # We need to force the names
  names(tbl_x_test) <- make.names(names = as.character(tbl_features$V2), unique = TRUE)
  
  # Join all the tables by column
  result_test <- cbind(tbl_y_test,tbl_subject_test,tbl_x_test)
  
}

extract_train <- function () {
  require (dplyr)
  
  #Create the data frames
  tbl_y_train <- tbl_df(read.table(file = "./data/UCI HAR Dataset/train/y_train.txt"))
  tbl_subject_train <- tbl_df(read.table(file = "./data/UCI HAR Dataset/train/subject_train.txt"))
  tbl_x_train <- tbl_df(read.table(file = "./data/UCI HAR Dataset/train/X_train.txt"))
  tbl_features <- tbl_df(read.table(file = "./data/UCI HAR Dataset/features.txt"))
  
  #name the tables
  names(tbl_y_train) <- "ActivityNumber"
  names(tbl_subject_train) <- "SubjectNumber"
    # We need to force the names
  names(tbl_x_train) <- make.names(names = as.character(tbl_features$V2), unique = TRUE)
  
  # Join all the tables by column
  result_train <- cbind(tbl_y_train,tbl_subject_train,tbl_x_train)
  
}

join_test_train <- function() {
  require (dplyr)
 
  # Join the rows from the 2 sources without printing it
  result_join <- rbind(extract_test(), extract_train())
  
}

filter_mean_std <- function() {
  
  require(dplyr)
  
  # Generate the main table
  tbl <- join_test_train()
  
  # Select only the mean and std relevant columns
  columnsMeanStd <- grep(names(tbl), pattern="mean|std", value = FALSE)
  tbl <- select(tbl, ActivityNumber, SubjectNumber, columnsMeanStd)
  
}

name_activities <- function() {
  
  require(dplyr)
  
  # Extract the activity numbers
  tbl <- filter_mean_std()
  activities_number <- select(tbl, ActivityNumber)
  activities_labels <- read.table(file = "./data/UCI HAR Dataset/activity_labels.txt")
  
  # Create a list with the names in the place of the numbers
  activities_name <- lapply(activities_number, function(x) {activities_labels$V2[x]})
  names(activities_name) <- "ActivityName"
  
  # Reconstruct the table with the new activity names & deleting the activity numbers
  tbl <- cbind(tbl,activities_name)
  tbl <- select(tbl, -ActivityNumber)
  
}

tidy_names <- function () {
  
  require(dplyr)
  
  #We are going to follow the UpperCamelCase naming convention
  
  #Get the table
  tbl <- name_activities()
  
  #Tidy the spatial dimension
  names(tbl) <- unlist(lapply(names(tbl),function(x){sub(x = x,pattern="^f", replacement = "Frequency")}))
  names(tbl) <- unlist(lapply(names(tbl),function(x){sub(x = x,pattern="^t", replacement = "Time")}))
  
  #Tidy the .mean and .std
  names(tbl) <- unlist(lapply(names(tbl),function(x){sub(x = x,pattern=".std", replacement = "Std")}))
  names(tbl) <- unlist(lapply(names(tbl),function(x){sub(x = x,pattern=".mean", replacement = "Mean")}))
  
  result <- tbl
  
}

average_bygroup <- function () {
  
  require(dplyr)
  
  #Get the tqble
  tbl <- tidy_names()
  
  #Group the data
  tbl <- group_by(tbl, ActivityName, SubjectNumber)
  
  #Generate the table of the means
  tbl <- summarise_each(tbl,funs(mean))
  
}