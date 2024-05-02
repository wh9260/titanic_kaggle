#Load libraries

library('tidyr')
library(dplyr)
library(ggplot2)
rm(list = ls())

#Load data

training <- read.csv('./data/train.csv')

training$Sex <- as.factor(training$Sex)
training$Pclass <- as.factor(training$Pclass)
training$Embarked <- as.factor(training$Embarked)

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
