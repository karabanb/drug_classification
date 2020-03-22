
### LIBRARIES ##########################################################################################################

library(tidyverse)

### LOADING DATA #######################################################################################################

file_url <- file('https://archive.ics.uci.edu/ml/machine-learning-databases/00373/drug_consumption.data')
raw_data <- read.table(file = file_url, sep = ',')
save(raw_data, file = 'data/100_raw_data.RData')
load('data/100_raw_data.RData')

### DATA PREPROCESSING #################################################################################################

anyNA(raw_data)       # Checking if any NA exists in data: FALSE


# helper vectors

Targets <- paste0('V', 14:32)
non_users <- c('CL0', 'CL1')
users <- c('CL2', 'CL3', 'CL4', 'CL5', 'CL6')



# Some variables are categorical, so they must be recoded to factors
# Categories in Drug Usage variable are merged into two categories: user, non-user
# Variables are ranamed for better interpratation of further models

preprocessed_data <- raw_data %>% 
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
  rename(
         Neuroticism = V7,
         Extraversion = V8,
         OpennessToExperience = V9,
         Agreeableness = V10,
         Conscientiousness = V11,
         Impulsiveness = V12,
         SensationSeeing = V13,
         Alcohol = V14,
         Amphet = V15,
         Amyl = V16,
         Benzos = V17,
         Caffeine = V18,
         Cannabis = V19,
         Chocloate = V20,
         Coke = V21,
         Crack = V22,
         Extasy = V23,
         Heroine = V24,
         Ketamine = V25,
         LegalHighs = V26,
         LSD = V27,
         Meth = V28,
         Mushrooms = V29,
         Nicotine = V30,
         Semeron = V31,
         VSA = V32
         ) %>% 
  select(-V1, -V2, -V3, -V4, -V5, -V6)

# warning 'Unknown levels in `f`: CL5, CL6' can be ignored. 
# The reason is lack of CL5 and CL6 levels in Semeron variable

save(preprocessed_data, file = 'data/100_preprocessed_data.RData')

rm(list = ls())




