#Prelim

library(tidyr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(lessR)
library(forcats)
library(stringr)

rm(list = ls())

#Load, clean, and separate data

training <- read.csv('./data/train.csv')

training$Sex <- as.factor(training$Sex)
training$Pclass <- as.factor(training$Pclass)

training$Embarked[training$Embarked == ""] <- "S"

training$Embarked <- as.factor(training$Embarked)

df_num <- training[,c('Age','SibSp','Parch','Fare')]
df_cat <- training[,c('Survived','Pclass','Sex','Ticket','Cabin','Embarked')]

##Begin by looking at survival within the categorical data

#Survival by class

#Pivot table to summerise survival by ticket class
survial_by_class <- table(df_cat$Survived, df_cat$Pclass) %>%
    as.data.frame() %>%
    pivot_wider(names_from = Var2, values_from = Freq)

#Format data for stacked bar plot

survial_by_class_stacked <- pivot_longer(survial_by_class, !Var1, names_to = "Class", values_to = "Freq")

ggplot(data = survial_by_class_stacked, aes(x = Class, y = Freq, fill = Var1)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_fill_manual(labels = c("Did not survive", "Survived"), values = c("darksalmon","darkseagreen")) +
    scale_x_discrete(labels = c("First", "Second", "Third")) +
    guides(fill=guide_legend(title=NULL)) +
    ylab("Percentage") +
    scale_y_continuous(labels = function(x) paste0(x*100, "%"))

ggsave("Survival by Ticket.png", path = "./code/")

#Survival by sex

#Pivot table to summerize survival by sex
survial_by_sex <- table(df_cat$Survived, df_cat$Sex) %>%
    as.data.frame() %>%
    pivot_wider(names_from = Var2, values_from = Freq)

#Format data for stacked bar plot

survial_by_sex_stacked <- pivot_longer(survial_by_sex, !Var1, names_to = "Sex", values_to = "Freq")

ggplot(data = survial_by_sex_stacked, aes(x = Sex, y = Freq, fill = Var1)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_fill_manual(labels = c("Did not survive", "Survived"), values = c("darksalmon","darkseagreen")) +
    scale_x_discrete(labels = c("Female", "Male")) +
    guides(fill=guide_legend(title=NULL)) +
    ylab("Percentage") +
    scale_y_continuous(labels = function(x) paste0(x*100, "%"))

ggsave("Survival by Sex.png", path = "./code/")

#Survival by embarkation

#Pivot table to summerize survival by embarkation port
survial_by_embark <- table(df_cat$Survived %>% factor(), df_cat$Embarked) %>%
    as.data.frame() %>%
    pivot_wider(names_from = Var2, values_from = Freq)

#Format data for stacked bar plot

survial_by_embark_stacked <- pivot_longer(survial_by_embark, !Var1, names_to = "Port", values_to = "Freq")

ggplot(data = survial_by_embark_stacked, aes(x = Port, y = Freq, fill = Var1)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_fill_manual(labels = c("Did not survive", "Survived"), values = c("darksalmon","darkseagreen")) +
    scale_x_discrete(labels = c("Cherborg", "Queensbury", "Southampton")) +
    guides(fill=guide_legend(title=NULL)) +
    ylab("Percentage") +
    scale_y_continuous(labels = function(x) paste0(x*100, "%"))

ggsave("Survival by Port of Embarkation.png", path = "./code/")
    

##Now look some feature engineering ideas.

#Cabin. Simplify cabins and look for impact upon survival of multiple cabins. How many cabins per ticket.

for (i in 1:length(df_cat$Cabin)){
    
    if (df_cat$Cabin[i] == ""){
        df_cat$cabin_multiple[i] <- 0
    }
    else {
        df_cat$cabin_multiple[i] <- length(str_split(df_cat$Cabin[i], " ")[[1]])
    }
}

cabin_multiple_table <- as.data.frame(table(df_cat$cabin_multiple))

print(cabin_multiple_table)

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

#Count number of cabin types

cabin_adv_table <- as.data.frame(table(df_cat$cabin_adv))

ggplot(data = cabin_adv_table, aes(x = Var1, y = Freq)) +
    geom_bar(stat = "identity") +
    labs(title = "Titanic #Cabins", x = "#Cabins per ticket", y = "Count") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(breaks=seq(0,800,50))

#Pivot table showing survival by cabin letter

survival_by_cabin_adv <- table(df_cat$Survived, df_cat$cabin_adv) %>%
    as.data.frame() %>%
    pivot_wider(names_from = Var2, values_from = Freq)

#Look at letters within the ticket value, numeric or string.
#First, is numeric or not.

training$ticket_letters <- NA
training$numeric_ticket <- NA

for (i in 1:length(training$Ticket)){
    
    if (is.na(as.numeric(training$Ticket[i]))){
        
        training$ticket_letters[i] <- str_to_lower(str_remove_all(str_split_1(training$Ticket[i]," ")[1], "[/.]"))#training$Ticket[i] %>%
            # str_split_1(" ")[1] %>%
            # str_remove("[/.") %>%
            # str_to_lower()
        training$numeric_ticket[i] <- 0
    }
    else {
        training$numeric_ticket[i] <- 1
    }
}

survived_by_numeric_ticket <- table(training$numeric_ticket, training$Survived) %>%
    as.data.frame() %>%
    pivot_wider(names_from = Var1, values_from = Freq)

survived_by_ticket_letters <- table(training$Survived, training$ticket_letters) %>%
    as.data.frame() %>%
    pivot_wider(names_from = Var2, values_from = Freq)








