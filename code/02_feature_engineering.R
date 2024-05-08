library(tidyr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(lessR)
library(forcats)
library(stringr)

rm(list = ls())

training <- read.csv('./data/train.csv')

training$Sex <- as.factor(training$Sex)
training$Pclass <- as.factor(training$Pclass)
training$Embarked <- as.factor(training$Embarked)

training$Embarked[training$Embarked == ""] <- "S"

df_num <- training[,c('Age','SibSp','Parch','Fare')]
df_cat <- training[,c('Survived','Pclass','Sex','Ticket','Cabin','Embarked')]

#Cabin. Simplify cabins and look for impact upon survival of multiple cabins.

# try without loop
#df_cat$cabin_multiple <- length(str_split(df_cat$Cabin, " ")[[1]])

#try with loop

for (i in 1:length(df_cat$Cabin)){
    
    if (df_cat$Cabin[i] == ""){
        df_cat$cabin_multiple[i] <- 0
    }
    else {
        df_cat$cabin_multiple[i] <- length(str_split(df_cat$Cabin[i], " ")[[1]])
    }
}

cabin_multiple_table <- as.data.frame(table(df_cat$cabin_multiple))

ggplot(data = cabin_multiple_table, aes(x = Var1, y = Freq)) +
    geom_bar(stat = "identity") +
    labs(title = "Titanic #Cabins", x = "#Cabins per ticket", y = "Count") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(breaks=seq(0,800,50))

#Count how many of each cabin type (letter). Extract first chr of each $Cabin.

for (i in 1:length(df_cat$Cabin)){
    
    if (df_cat$Cabin[i] == ""){
        df_cat$cabin_adv[i] <- 'n'
    }
    else {
        df_cat$cabin_adv[i] <-  substring(df_cat$Cabin[i],1,1)
    }
}

cabin_adv_table <- as.data.frame(table(df_cat$cabin_adv))

cabin_adv_table <- cabin_adv_table[order(cabin_adv_table$Var1),]

ggplot(data = cabin_adv_table, aes(x = Var1, y = Freq)) +
    geom_bar(stat = "identity") +
    labs(title = "Titanic #Cabins", x = "#Cabins per ticket", y = "Count") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(breaks=seq(0,800,50))


