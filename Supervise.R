# load the packages
library(tidyverse)
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
#install.packages("Hmics")
#install.packages("funModeling")
library(Hmics)
library(funModeling)
install.packages(c("FactoMineR", "factoextra"))
library(FactoMineR,factoextra)
install.packages("FactorMineR, factorextra")
library(tidyverse)
install.packages("packageIQR")
install.packages("PCAmixdata")
library(PCAmixdata)

## read the dataset 
data <- read.csv("../Unsupervised-Machine-Learning/project_train.csv")
glimpse(train)
sml = data %>% select(-issue_d, -train, -education)
### sml 是简单删减后的原数据，用于做


### MFA
get_mfa(sml)
names(sml)
View(sml)
View(data)

res.famd <- FAMD(sml)
data(sml)
numeric_data <- data %>% select(emp_length_int, 
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
cols <- c("loan_condition_cat"        
         ,"munster",                  
          "leinster",                 
         "cannught",                  
         "ulster",                    
         "Northern",                  
         "rent",                      
         "own",                       
         "mortgage",                  
         "credit_card",               
         "car",                       
          "medical",                   
          "house",                     
         "small_business",            
          "vacation",                  
          "application_type_cat_dummy",
         "interest_payment_cat",
         "grade")

orange[cols] <- lapply(data[cols], factor)
orange = cbind(binary, numeric_data)
data(gironde)
View(gironde)
splite <- splitmix(orange)
X1<-split$X.quanti
X2<-split$X.quali
View(orange)
class(orange$loan_condition_cat)

View(binary)
names(orange)
num

obj <- PCAmix(X.quanti=numeric_data,X.quali=binary,ndim=2)
plot(obj, choice = "imd")
head(obj$quanti.cor)

binary <- binary %>%
  mutate(ifelse(RainToday == "No",0,1))
