#Load libraries

library(tidyr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(lessR)
library(forcats)



rm(list = ls())

#Load data

training <- read.csv('./data/train.csv')

training$Sex <- as.factor(training$Sex)
training$Pclass <- as.factor(training$Pclass)
training$Embarked <- as.factor(training$Embarked)

# Replace the two embarkation data missing, confirmed from Southampton

training$Embarked[training$Embarked == ""] <- "S"

#Create a pivot table to compare survival to sex

df1 <- training %>%group_by(Sex,Survived)%>%summarise(count = n())
print(df1)
df1$Sex <- as.factor(df1$Sex)
df1$Survived <- as.factor(df1$Survived)
ggplot(data = df1, aes(fill=Survived, x = Sex, y = count)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_fill_discrete(name = "Survived?", labels = c("No", "Yes")) +
    labs(title = "Titanic Deaths by Sex", x = "Sex", y = "Count") +
    scale_x_discrete(labels = c("Female", "Male")) +
    theme(plot.title = element_text(hjust = 0.5))

#Split data into categorical and numeric typed dfs

df_num <- training[,c('Age','SibSp','Parch','Fare')]
df_cat <- training[,c('Survived','Pclass','Sex','Ticket','Cabin','Embarked')]

#Plot histograms for each numeric variables
breaks = seq(from = 0, to = 90, by = 1)
hist(training$Age, breaks = breaks)
#training %>% count(Age)

#breaks = seq(from = 0, to = 10, by = 1.0)
#hist(training$SibSp, breaks = breaks)
#training %>% count(SibSp)

ggplot(training, aes(x=SibSp)) +
    geom_histogram(bins = 10) +
    scale_x_continuous(breaks  = seq(0,10,1))

breaks = seq(from = -0.5, to = 10.5, by = 1)
hist(training$Parch, breaks = breaks)


#Look at correlation between numeric variables.
corrplot(cor(df_num, use = "complete.obs"), method = 'color', diag = 0)


pv_Age <- pivot(training, mean, Age, Survived)
pv_Fare <- pivot(training, mean, Fare, Survived)
pv_Parch <- pivot(training, mean, Parch, Survived)
pv_SibSp <- pivot(training, mean, SibSp, Survived)


#Plot 6 bar charts for the categorical data
#1 - Overall survived

ggplot(data = count(training, Survived), aes(x = Survived, y = n)) +
    geom_bar(stat = "identity") +
    labs(title = "Titanic Deaths", x = "Survived", y = "Count") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(breaks=seq(0,800,50))

#2 - Pclass

ggplot(data = count(training,Pclass), aes(x = Pclass, y = n)) +
    geom_bar(stat = "identity") +
    labs(title = "Titanic Deaths by Class", x = "Class", y = "Count") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_discrete(labels = c("First", "Second", "Third")) +
    geom_text(aes(label = n), vjust = -0.4)

#3 - Sex

ggplot(data = count(training,Sex), aes(x = Sex, y = n)) +
    geom_bar(stat = "identity") +
    labs(title = "Titanic Deaths by Sex", x = "Sex", y = "Count") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_discrete(labels = c("Male", "Female")) +
    geom_text(aes(label = n), vjust = -0.4)

#4 - Ticket

training %>% count(Ticket) %>% mutate(Ticket = fct_rev(fct_reorder(Ticket,n))) %>% ggplot(aes(x = Ticket, y = n)) +
    geom_bar(stat = "identity") +
    labs(title = "Titanic Deaths by Ticket", x = "Ticket", y = "Count") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_text(aes(label = n), vjust = -0.4)

#5 - Cabin

training %>% count(Cabin) %>% mutate(Cabin = fct_rev(fct_reorder(Cabin,n))) %>% ggplot(aes(x = Cabin, y = n)) +
    geom_bar(stat = "identity") +
    labs(title = "Titanic Deaths by Cabin", x = "Cabin", y = "Count") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_text(aes(label = n), vjust = -0.4)

#6 - Embarked

training %>% count(Embarked) %>% mutate(Embarked = fct_rev(fct_reorder(Embarked,n))) %>% ggplot(aes(x = Embarked, y = n)) +
    geom_bar(stat = "identity") +
    labs(title = "Titanic Deaths by Cabin", x = "Embarked", y = "Count") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_text(aes(label = n), vjust = -0.4)

    









