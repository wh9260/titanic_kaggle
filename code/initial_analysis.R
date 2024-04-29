#Load libraries

library(dplyr)
rm(list = ls())

#Load data

training <- read.csv('./data/train.csv')

training$Sex <- as.factor(training$Sex)
training$Pclass <- as.factor(training$Pclass)
training$Embarked <- as.factor(training$Embarked)

#Create a pivot table to compare survival to sex

df1 <- training %>%group_by(Sex,Survived)%>%summarise(count = n())
df1$Sex <- as.factor(df1$Sex)
df1$Survived <- as.factor(df1$Survived)
ggplot(data = df1, aes(fill=Survived, x = Sex, y = count)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_fill_discrete(name = "Survived?", labels = c("No", "Yes")) +
    labs(title = "Titanic Deaths by Sex", x = "Sex", y = "Count") +
    scale_x_discrete(labels = c("Female", "Male")) +
    theme(plot.title = element_text(hjust = 0.5))

