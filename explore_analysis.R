### Interesting analysis about data 
install.packages("Hmisc")
install.packages("funModeling")
install.packages("DataExplorer")
library(Hmisc)
library(funModeling)
library(DataExplorer)
train <- read.csv(file = "project_train.csv")

str(train)
summary(train)
View(train)
max(train$annual_inc)


min(train$annual_inc)
max(train$loan_amount)
min(train$loan_amount)

# 0-100000 low income, 100001- 200000 medum income, above 2000001
library(tidyverse)
library(ggplot2)

low <- train %>% select(annual_inc, loan_amount) %>% filter(annual_inc <= 100000)
medium <- train %>% select(annual_inc, loan_amount) %>%filter(annual_inc >= 100001 & annual_inc <= 200000)
high <- train %>%  select(annual_inc, loan_amount) %>%filter(annual_inc >= 200001)

count(low)
count(medium)
count(high)
train_tep <- train
train_tep$grade[train_tep$grade == 1] <- "G"
train_tep$grade[train_tep$grade == 2] <- "F"
train_tep$grade[train_tep$grade == 3] <- "E"
train_tep$grade[train_tep$grade == 4] <- "D"
train_tep$grade[train_tep$grade == 5] <- "C"
train_tep$grade[train_tep$grade == 6] <- "B"
train_tep$grade[train_tep$grade == 7] <- "A"


#### graphs
freq(train_tep$grade)

train_tep_1 <- train %>% select (loan_amount, interest_rate)
plot_num(train_tep_1)

plot_correlation(train, type = 'continuous','Review.Date')

