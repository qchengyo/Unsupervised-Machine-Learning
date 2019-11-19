options(stringsAsFactors = FALSE)

library(readr)
library(tidyverse)
library(factoextra)
train <- read.csv("../Unsupervised-Machine-Learning/project_test.csv")
glimpse(train)
train<-train %>% select(-X, -issue_d, -train, -education)
## PCA with only with non birary variable
numeric_data <- train %>% select(emp_length_int, 
                        annual_inc, 
                        loan_amount, 
                        interest_payment_cat,
                        interest_rate,
                        grade,
                        dti,
                        total_pymnt,
                        total_rec_prncp,
                        recoveries,
                        installment,
                        )
glimpse(numeric_data)
skimr::skim(numeric_data)
## create a dataset with only binary variable with 17 columns
binary <- train %>% select(loan_condition_cat,munster:application_type_cat_dummy)
glimpse(binary)

## Princinple Components Analysis
## option 2: Keep only numeric variable to fit the pca
train_pca = prcomp(numeric_data, center = TRUE, scale = TRUE)
names(train_pca)

## summary the pca
summary(train_pca)
get_eigenvalue(train_pca)
## compute standard deviation of each principal component
train_dev<-train_pca$sdev
## compute variance
train_var <- train_dev^2
## proportion of variance explained
train_varex <- train_var/sum(train_var)
## scree plot
plot(train_varex, xlab="Principle Components",
        ylab = "Proportion of Variance Explained",
        type = "b")

## cumulative scree plot
plot(cumsum(train_varex), xlab="Principle Components",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

## Another way to show scree plot
fviz_screeplot(train_pca, addlabels = TRUE)

## Option 3: PCA with all variable without scale
pca_all_without_scale = prcomp(train, center = TRUE, scale = FALSE)
get_eigenvalue(pca_all_without_scale)
## Option 4: PCA with all variables
## with binary variable dataset
pca_all_with_scale = prcomp(train, center = TRUE, scale = TRUE)
get_eigenvalue(pca_all_with_scale)
## The result shows that we need to include at least 13 princinple components which can explain 75% of variance, so this is not the 
## ideal option to choose 
## Let's stick with option 2

###### Based the result, we don't think we should do the pca with binary variables. Because the result does not make sense to us####

############---------------------------------------------------------------------------------------------------------##############
## option 5
## scale numeric only, combine to binary variable and do pca
scale_numeric = scale(numeric_data)

## combine with binary data
option5_data = cbind(scale_numeric, binary)
glimpse(option5_data)
## fit the pca model
pca_option5 = prcomp(option5_data, center = TRUE, scale = FALSE)
summary(pca_option5)
get_eigenvalue(pca_option5)

#### The result shows that first four princinple component only explain 64% of variance which is less than the option 2

######################----------------------------------------------------------------------##########################################
### Stick with the option 2 pca model which can explain 74% of variance and with eigenvalue 1, 
### we decide to use this pca option to combine with binary variable for 
## the further analysis
get_eigenvalue(train_pca)

## extract the first princinple component from option 2
pca_fit = predict(train_pca, newdata = train)
pca_fit = pca_fit[,1:4]
pca_fit = as.data.frame(pca_fit)

## combine the pca and binary to one dataframe
train_new <- cbind(binary, pca_fit)
glimpse(train_new)



### Clustering Analysis
## Use silhouette and wss method to get a sense of the number of clusters
## try option 5 PCA and extract first 9 PCA

option5_pca9 = pca_option5$x[,1:9]
k = kmeans(option5_pca9, center = 5, iter.max = 25, nstart = 25)
fviz_cluster(k,train)

