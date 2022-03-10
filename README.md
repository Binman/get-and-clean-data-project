# get-and-clean-data-project
final peer-viewed assignment
#load library
library(tidyverse)

#a simple function to convert numbers in activity_labels.txt into
#descriptive names
lab_covert <- function(x){
  if (x == 1) {
    x <- 'WALKING'
    #print(x)
  } else if (x ==2) {
    x <- 'WALKING_UPSTRAIRS'
    #print(x)
  } else if (x ==3) {
    x <- 'WALKING_DOWNSTRAIRS'
    #print(x)
  } else if (x ==4) {
    x <- 'SITTING'
    #print(x)
  } else if (x ==5) {
    x <- 'STANDING'
    #print(x)
  } else if (x ==6) {
    x <- 'LAYING'
    #print(x)
  } 
}

#read test dataset and test labs
test_data <- read_table('./UCI HAR Dataset/test/X_test.txt', col_names = F, 
                   cols(.default = col_double()))
test_lab <- read_table('./UCI HAR Dataset/test/y_test.txt', col_names = F,
                       col_types = 'd')
#convert test labs into names
lab1 <- test_lab$X1 %>% 
  map(lab_covert) %>% 
  flatten() %>% 
  unlist() %>% 
  as.data.frame()
#combine labs with dataset
test <- cbind(lab1, test_data)


#read train dataset and train labs
train_data <- read_table('./UCI HAR Dataset/train/X_train.txt', col_names = F,
                    cols(.default = col_double()))
train_lab <- read_table('./UCI HAR Dataset/train/y_train.txt', col_names = F,
                        col_types = 'd')
#convert train labs into names
lab2 <- train_lab$X1 %>% 
  map(lab_covert) %>% 
  flatten() %>% 
  unlist() %>% 
  as.data.frame()
#combine train dataset with labs
train <- cbind(lab2, train_data)

#merge train and test datasets
dt <- rbind(test, train)

#read features from features.txt file
colnames <- read.table('./UCI HAR Dataset/features.txt', header = F,
                       sep = ' ')
#extract descriptive names and add extra label
colnames <- colnames[ ,2]
colnames <- append(colnames, 'label', after = 0)
#labels the data set with descriptive variable names
names(dt) <- colnames

#extract required cols
require_cols <- names(dt) %>% 
  map(., ~ str_detect(., '(mean|std)')) %>% 
  unlist()
#keep the first col which is lab
require_cols[1] <- 'TRUE'
require_cols <- as.logical(require_cols)
#create new dataframe for required cols
selected <- dt %>% 
  select(which(require_cols))

#calculate the average of each variable for each activity and each subject.
finaldata <- selected %>% 
  group_by(label) %>% 
  summarise(across(everything(), mean))

write.table(finaldata, 'finaldata.txt', row.names = F)




