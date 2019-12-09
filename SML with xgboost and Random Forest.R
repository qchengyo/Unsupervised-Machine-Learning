# load the packages
library(tidyverse)
library(xgboost)
library(rpart)
library(factoextra)
library(corrplot)
library(psych)
library(GPArotation)
library(dplyr)
library(ggplot2)
library(readxl)
# install.packages("dummies")
library(dummies)    ## easier to dummy encode categorical data
# install.packages("philentropy")
library(philentropy)   ## expanded distance calculations
# install.packages("skimr")
library(skimr)     ## easy summary statistics
# install.packages("cluster")
library(cluster)    ## datasets and utilities for clustering, along with some algos
# install.packages("factoextra")
library(factoextra)    ## clustering visualization utilities
# install.packages("dendextend")
library(dendextend)     ## for working with dendrograms
library(devtools) 
# install.packages("Hmics")
# install.packages("funModeling")
library(Hmics)
library(funModeling)
library(randomForest)
### Clean data
loan<- read.csv("~/Desktop/BA 820/Team Project /loan_final313.csv")

loan <- loan %>% filter(purpose == "credit_card"|purpose == "card"| purpose == "medical"|
                          purpose == "house" | purpose == "small_business"| purpose == "vacation")
loan <- loan %>% filter(home_ownership == "RENT"|home_ownership=="OWN"| home_ownership == "MORTGAGE")
loan <- loan %>% filter(region == "munster"| region == "leinster"|region == "cannught"|region == "Ulster"| region == "Northern Ireland")
unique(loan$home_ownership)

loan_clean <- loan %>% select(-1:-4,-7,-8, -10,-13,-15,-17,-19,-20,-21,-23:-24)

loan_lda <- loan %>% select(-1:-4,-6,-8,-12,-14,-16,-18,-20, -23:-24,-30)
glimpse(loan_lda)

set.seed(888)
sample = sample(1:nrow(loan_lda), 87518)
train = loan_lda[sample, ]# %>% select(home_ownership_cat,loan_condition_cat,income_cat, interest_rate,purpose_cat)
test = loan_lda[-sample, ]# %>% select(home_ownership_cat,loan_condition_cat,income_cat, interest_rate, purpose_cat)
head(train)
head(test)
train_x = train %>% select(-loan_condition_cat) %>% as.matrix()
train_y = train %>% select(loan_condition_cat) %>% as.matrix()
test_x = test %>% select(-loan_condition_cat) %>% as.matrix()
test_y = test %>% select(loan_condition_cat) %>% as.matrix()


xgmod = xgboost(data = train_x,
                label = train_y,
                objective = "multi:softmax",
                num_class = 5,
                nrounds = 15)

preds = predict(xgmod, test_x)
xgtab = table(preds, test_y)
yardstick::accuracy(xgtab)

test <- chisq.test(train$recoveries , train_y)
print(test)

#### logistic 
model_glm <- train(loan_condition_cat~.,
                   data = train,
                   method = "glm",
                   family = "binomial")


### predict to test abd check the accuracy
preds = predict(xgmod, test_x)
xgtab = table(preds, test_y)
yardstick::accuracy(xgtab)

#### random forest 
f2 = as.formula(loan_condition_cat ~. )

condition_rf <- randomForest(f2, train, ntree = 200, do.trace=F) 
yhat_rf_train <- predict(condition_rf, train)
mse_rf_train <- mean((yhat_rf_train - train$loan_condition_cat) ^2) 
yhat_rf_test <- predict(condition_rf, test)
mse_rf_test <- mean((yhat_rf_test - test$loan_condition_cat) ^2) 
varImpPlot(condition_rf)
mean(bank$duration)

paste("Random Forest Train MSE",mse_rf_train)
paste("Random Forest Test MSE",mse_rf_test)
               
