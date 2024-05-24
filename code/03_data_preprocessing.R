#Prelim
library(klaR)
library(caret)
library(tidyr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(lessR)
library(forcats)
library(stringr)

rm(list = ls())

#Concat training and test into alldata

training <- read.csv('./data/train.csv')
test <- read.csv('./data/test.csv')

#Indicate which should be used for training
training$train_test <- 1
test$train_test <- 0

#Not know as need to be predicted
test$Survived <- NaN

#Join
alldata <- rbind(training, test)

#Fill empty ages with median

alldata$Age[is.na(alldata$Age)] <- median(alldata$Age, na.rm = TRUE)

#Fill empty fares with median

alldata$Fare[is.na(alldata$Fare)] <- median(alldata$Fare, na.rm = TRUE)

#Find logs of SibSq and fare

alldata$norm_sibsq <- log(alldata$SibSp + 1)
alldata$norm_fare <- log(alldata$Fare + 1)

#Convert fare to str type
alldata$Pclass <- as.factor(alldata$Pclass)

alldata$Embarked <- as.factor(alldata$Embarked)

#Recreate feature engineering columns as per 02_...

#Cabin multiple

for (i in 1:length(alldata$Cabin)){
    
    if (alldata$Cabin[i] == ""){
        alldata$cabin_multiple[i] <- 0
    }
    else {
        alldata$cabin_multiple[i] <- length(str_split(alldata$Cabin[i], " ")[[1]])
    }
}

#Cabin adv

for (i in 1:length(alldata$Cabin)){
    
    if (alldata$Cabin[i] == ""){
        alldata$cabin_adv[i] <- 'n'
    }
    else {
        alldata$cabin_adv[i] <-  substring(alldata$Cabin[i],1,1)
    }
}

#Numeric ticket and ticket letters

alldata$ticket_letters <- NA
alldata$numeric_ticket <- NA

for (i in 1:length(alldata$Ticket)){
    
    if (is.na(as.numeric(alldata$Ticket[i]))){
        
        alldata$ticket_letters[i] <- str_to_lower(str_remove_all(str_split_1(alldata$Ticket[i]," ")[1], "[/.]"))#training$Ticket[i] %>%
        # str_split_1(" ")[1] %>%
        # str_remove("[/.") %>%
        # str_to_lower()
        alldata$numeric_ticket[i] <- 0
    }
    else {
        alldata$numeric_ticket[i] <- 1
    }
}

#Name title

for (i in 1:length(alldata$Name)){
    
    alldata$name_title[i] <- str_split_i(str_split_1(alldata$Name[i], "[.]")[1], " ",-1)
    
}


alldata$cabin_adv <- as.factor(alldata$cabin_adv)

#Create training and test sets
#Training set without Survived

training_set <- alldata %>% filter(train_test == 1)
test_set <- alldata %>% filter(train_test == 0)

save(alldata, file = "./data/alldata.Rda")
save(training_set, file = "./data/training_set.Rda")
save(test_set, file = "./data/test_set.Rda")

features <- setdiff(names(training_set), c("Survived", "Name", "PassengerId", "Ticket", "Cabin", "ticket_letters", "train_test"))

x <- training_set[,features]
y <- as.factor(training_set$Survived)

#Naive Bayes

train_control <- trainControl(method = "cv", number = 10)

nb.m1 <- train(x = x, y = y, method = "nb", trControl = train_control)

confusionMatrix(nb.m1)

#Linear regression

train_control <- trainControl(method = "cv", number = 4)

nb.m2 <- train(x = x, y = y, method = "glm", trControl = train_control)

confusionMatrix(nb.m2)


