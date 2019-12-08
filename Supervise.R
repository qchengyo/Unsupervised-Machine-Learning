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
#install.packages(c("FactoMineR", "factoextra"))
library(FactoMineR,factoextra)

## read the dataset 
data <- read.csv("../Unsupervised-Machine-Learning/project_test.csv")
glimpse(train)
sml = data %>% select(-issue_d, -train, -education)
install.packages(c("FactoMineR", "factoextra"))
### sml 是简单删减后的原数据，用于做


