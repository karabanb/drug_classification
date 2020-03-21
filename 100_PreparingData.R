

### LIBRARIES ##########################################################################################################

library(tidyverse)
library(funModeling)

### LOADING DATA #######################################################################################################

file_url <- file('https://archive.ics.uci.edu/ml/machine-learning-databases/00373/drug_consumption.data')
raw_data <- read.table(file = file_url, sep = ',')
save(raw_data, file = 'data/100_raw_data.RData')
load('data/100_raw_data.RData')

### DATA PREPROCESSING #################################################################################################

anyNA(raw_data)       # Checking if any NA exists in data: FALSE


# helper vectors

Targets <- paste0('V', 14:32)
users <- c('CL0', 'CL1')
non_users <- c('CL2', 'CL3', 'CL4', 'CL5', 'CL6')



tmp <- raw_data %>% 
  mutate(Age = factor(V2, sort(unique(V2)),
                      labels = c('18-24', '25-34', '35-44', '45-54', '55-64', '65 +')),
         Gender = factor(V3, sort(unique(V3)),
                         labels = c('M', 'F')),
         Education = factor(V4, sort(unique(V4)),
                            labels = c(1:length(unique(V4)))),
         Country = factor(V5, sort(unique(V5)),
                          labels = c('USA', 'New Zeland', 'Other', 'Australia', 'Ireland', 'Canada', 'UK')),
         Ethnicity = factor(V6, sort(unique(V6)),
                            labels = c('Black', 'Asian', 'White', 'Mixed W/B', 'Other', 'Mixed W/A', 'Mixed B/A'))) %>% 
  mutate_at(Targets, fct_collapse, non_user = non_users, user = users) %>%
  select(-V1, -V2, -V3, -V4, -V5, -V6)




