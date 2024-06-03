#Prelim
library(tidymodels)
library(klaR)
library(caret)
library(tidyr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(lessR)
library(forcats)
library(stringr)
library(rpart)
library(rpart.plot)
library(discrim)
library(purrr)
library(doParallel)
library(kknn)

#Clear environment
rm(list = ls())

##Concatenate training and test into alldata

#Load files
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

#Factorise other variable
alldata$Embarked <- as.factor(alldata$Embarked)
alldata$Sex <- as.factor(alldata$Sex)
alldata$cabin_adv <- as.factor(alldata$cabin_adv)
alldata$name_title <- as.factor(alldata$name_title)
alldata$cabin_multiple <- as.factor(alldata$cabin_multiple)
alldata$numeric_ticket <- as.factor(alldata$numeric_ticket)
alldata$Pclass <- as.factor(alldata$Pclass)


#Create training and test sets
#Training set without Survived

training_set <- alldata %>% filter(train_test == 1)
test_set <- alldata %>% filter(train_test == 0)

training_set$Survived <- as.factor(training_set$Survived)

save(alldata, file = "./data/alldata.Rda")
save(training_set, file = "./data/training_set.Rda")
save(test_set, file = "./data/test_set.Rda")

##Apply Tidymodel approach to analysis

#Define which features are used to model. Those in c() are not used.

feat <- setdiff(names(training_set), c("PassangerId", "Name", "Ticket", "Cabin", "ticket_letters", "train_test"))
x <- training_set[, feat]

#Naive Bayes

nb_spec <- naive_Bayes() %>%
    set_mode("classification") %>%
    set_engine("klaR")%>%
    set_args(usekernel = FALSE)

nb_fit <- nb_spec %>%
    fit(Survived ~ ., data = x)

#Logistic regression

lr_spec <- logistic_reg() %>%
    set_engine("glm") %>%
    set_mode("classification")

lr_fit <- lr_spec%>%
    fit(Survived ~ ., data = x)

lr_fit %>%
    pluck("fit") %>%
    summary()

#Decision tree

dt_spec <- decision_tree() %>%
    set_engine("rpart") %>%
    set_mode("classification")

dt_fit <- dt_spec %>%
    fit(Survived ~ ., data = x)

dt_fit %>%
    extract_fit_engine() %>%
    rpart.plot()

dt_fit %>%
    pluck("fit") %>%
    summary()

augment(dt_fit, new_data = x) %>%
    accuracy(truth = Survived, estimate = .pred_class)

augment(dt_fit, new_data = x) %>%
    conf_mat(truth = Survived, estimate = .pred_class)




#Tunable decision tree
#Applying tuning from https://juliasilge.com/blog/wind-turbine/

dt_x <- x

dt_x$Survived_numeric <- as.integer(x$Survived)

dt2_folds <- vfold_cv(dt_x, strata = Survived_numeric)

dt2_spec <- decision_tree(
    cost_complexity = tune(),
    tree_depth = tune(),
    min_n = tune()
) %>%
    set_engine("rpart") %>%
    set_mode("regression")

dt2_grid <- grid_regular(cost_complexity(), tree_depth(), min_n(), levels = 4)

doParallel::registerDoParallel()

tree_rs <- tune_grid(
    dt2_spec,
    Survived_numeric ~ .,
    resamples = dt2_folds,
    grid = dt2_grid,
    metrics = metric_set(rmse, rsq, mae, mape)
)

#KNN

kknn_x <- x

kknn_spec <- nearest_neighbor() %>%
    set_engine("kknn") %>%
    set_mode("classification")

kknn_fit <- kknn_spec %>%
    fit(Survived ~ ., data = kknn_x)






