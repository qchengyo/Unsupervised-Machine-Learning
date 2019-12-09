library(tidyverse)
library(readxl)
library(forcats)

#Exlporing the clusters

clusterdata <- read_csv("820/Project/cluster assignments.csv")

glimpse(clusterdata)

clusterdata %>% 
  group_by(clusters) %>% 
  summarise(avgincome = mean(interest_rate))

clusterdata %>% 
  group_by(clusters) %>% 
  count()

clusterdata %>% 
  group_by(clusters) %>% 
  count(clusterdata$purpose)

cluster1 <- clusterdata %>% 
  filter(clusters == 1) 

fct_count(cluster1$purpose)

cluster2 <- clusterdata %>% 
  filter(clusters == 2)

fct_count(cluster2$purpose)

cluster3 <- clusterdata %>% 
  filter(clusters == 3)

fct_count(cluster3$purpose)

glimpse(cluster1)

fct_count(cluster1$home_ownership)
fct_count(cluster2$home_ownership)
fct_count(cluster3$home_ownership)

fct_count(cluster1$term)
fct_count(cluster2$term)
fct_count(cluster3$term)

clusterdata %>% 
  group_by(clusters) %>% 
  summarise(avg_total_payment = mean(total_pymnt))

clusterdata %>% 
  group_by(clusters) %>% 
  summarise(avg_installment = mean(installment))

clusterdata %>% 
  group_by(clusters) %>% 
  summarise(avg_installment = mean(dti))

