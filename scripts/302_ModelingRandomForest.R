
### LIBRARIES ##########################################################################################################

library(tidyverse)
library(mlr)
library(caret)
library(rpart)
library(rpart.plot)
library(ranger)
library(MLmetrics)


### PREPARING DATASET ##################################################################################################

load('data/200_cleaned_data.RData')
load('tmp/200_DrugNames.RData')
load('tmp/300_train_index.RData')
load('data/301_trees_data.RData')


### MODELING DECISION RANFOM FOREST ####################################################################################

classif_task <- makeClassifTask(id = 'rf',
                                data = select(trees_data[ix_trn,], -BenzosInt),
                                target = 'Benzos',
                                positive = 'user')

learner_rf <- makeLearner('classif.ranger',predict.type = 'prob')

cv_scheme <- makeResampleDesc(method = 'RepCV',
                              folds = 4,
                              reps = 20,
                              stratify = TRUE,
                              predict = 'both')


### Hyperparameter search space ----------------------------------------------------------------------------------------

parameters_set <- makeParamSet(
  makeIntegerParam("num.trees", lower = 300, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 6),
  makeIntegerParam("min.node.size", lower = 50, upper = 200)
)

ctrl <- makeTuneControlRandom(maxit = 40)


### Defining measures --------------------------------------------------------------------------------------------------

auc_trn <- setAggregation(auc, train.mean)
auc_sd <- setAggregation(auc, test.sd)

### Tuning model -------------------------------------------------------------------------------------------------------

set.seed(42)

rf_tune_results <- tuneParams(learner = learner_rf,
                           task = classif_task,
                           resampling = cv_scheme,
                           measures = list(auc_trn, auc, auc_sd),
                           par.set = parameters_set,
                           control = ctrl)


df_rf_tune_results <- data.frame(rf_tune_results$opt.path) %>%
  mutate(diff_auc = auc.train.mean - auc.test.mean) %>%
  arrange(diff_auc)


### Training with choosen hyparparameters ------------------------------------------------------------------------------ 

m6_rf <- ranger(Benzos ~.-BenzosInt,
                data = trees_data[ix_trn, ], 
                num.trees = 341,
                mtry = 5,
                min.node.size = 196,
                seed = 42,
                probability = TRUE)

save(m6_rf, file = 'results/302_m6_rf.RData')

### MEASURING PERFORMANCE ##############################################################################################

pred_m6_prob <- predict(m6_rf, data = trees_data)$predictions[,2]

pred_m6 <- data.frame(probs = pred_m6_prob,
                      category = ifelse(pred_m6_prob > 0.5, 'user', 'non_user'))


auc_rf_trn <- AUC(pred_m6$probs[ix_trn], trees_data[ix_trn, 'BenzosInt'])
auc_rf_tst <- AUC(pred_m6$probs[-ix_trn], trees_data[-ix_trn, 'BenzosInt'])

cm_m6_rf <- confusionMatrix(pred_m6$category[-ix_trn], trees_data[-ix_trn, 'Benzos'], positive = 'user')


perf_rf <- data.frame(model = 'random forest',
                      auc_train = auc_rf_trn,
                      auc_test = auc_rf_tst,
                      auc_test_sd = df_rf_tune_results[2, 'auc.test.sd'],
                      precision_test = cm_m6_rf$byClass['Precision'],
                      recall_test = cm_m6_rf$byClass['Recall'])

save(cm_m6_rf, perf_rf, file = 'tmp/302_rf_performace.RData')
rm(list = ls())



