
### LIBRARIES ##########################################################################################################

library(tidyverse)
library(mlr)
library(caret)
library(car)
library(MLmetrics)


### LOADING DATA #######################################################################################################

load('data/200_cleaned_data.RData')
load('tmp/200_DrugNames.RData')


### PREPARING DATASET ##################################################################################################

glm_data <- cleaned_data %>%
  select(-setdiff(drugs, 'Benzos')) %>% 
  mutate(BenzosInt = if_else(Benzos == 'user', 1, 0)) %>%   # some libs for maesuring AUC requires Target as numeric
  createDummyFeatures(obj = ., target = 'Benzos')

set.seed(42)
ix_trn <- createDataPartition(glm_data$Benzos, p = 0.7, list = FALSE)

save(ix_trn, file = 'tmp/300_train_index.RData')
save(glm_data, file = 'data/300_glm_data.RData')


### MODELING ###########################################################################################################

m1_glm <- glm(Benzos ~. - BenzosInt, data = glm_data, subset = ix_trn, family = binomial)
m1_glm <- step(m1_glm, direction = 'both')

summary(m1_glm)

vif(m1_glm)

m2_glm <- glm(Benzos ~ Neuroticism + OpennessToExperience + SensationSeeing + 
            Age.25.34 + Age.35.44 + Age.45.54 + Gender.M + 
            Education.4 + Country.USA,
          data = glm_data,
          subset = ix_trn,
          family = 'binomial')


vif(m2_glm)
summary(m2_glm)

m3_glm <- step(m2_glm, direction = 'both')
summary(m3_glm)

save(m3_glm, file = 'results/300_m3_glm.RData')


### PREDICTION #########################################################################################################

pred_m3 <- data.frame(probs = predict(m3_glm, newdata = glm_data, type = 'response'),
                      category = ifelse(predict(m3_glm, newdata = glm_data, 'response') > 0.5, 'user', 'non_user'))


### PERFORMANCE ########################################################################################################

AUC(pred_m3$probs[ix_trn], glm_data[ix_trn, 'BenzosInt'])
AUC(pred_m3$probs[-ix_trn], glm_data[-ix_trn, 'BenzosInt'])

confusionMatrix(pred_m3$category[-ix_trn], glm_data[-ix_trn, 'Benzos'], positive = 'user')


### Performing Cross Validation for checking standard deviation of predicted AUC ---------------------------------------

glm_fited_data <- glm_data %>% 
  select(Benzos,
         Neuroticism,
         OpennessToExperience,
         SensationSeeing,
         Age.25.34,
         Age.35.44,
         Age.45.54,
         Gender.M,
         Country.USA)

classif_task <- makeClassifTask(id = 'glm',
                                data = glm_fited_data[ix_trn,],
                                target = 'Benzos',
                                positive = 'user')

learner_glm <- makeLearner('classif.logreg', predict.type = 'prob')

cv_scheme <- makeResampleDesc(method = 'RepCV',
                              folds = 4,
                              reps = 20,
                              stratify = TRUE,
                              predict = 'both')


### Defining measures --------------------------------------------------------------------------------------------------

auc_trn <- setAggregation(auc, train.mean)
auc_sd <- setAggregation(auc, test.sd)


set.seed(42)
resample_glm <- resample(learner = learner_glm,
                         task = classif_task,
                         resampling = cv_scheme,
                         measures = list(auc_trn, auc, auc_sd))

resample_glm

rm(list = ls())


