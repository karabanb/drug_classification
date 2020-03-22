
### LIBRARIES ##########################################################################################################

library(tidyverse)
library(mlr)
library(caret)
library(rpart)
library(rpart.plot)
library(MLmetrics)

### PREPARING DATASET ##################################################################################################

load('data/200_cleaned_data.RData')
load('tmp/200_DrugNames.RData')
load('tmp/300_train_index.RData')

trees_data <- cleaned_data %>% 
  select(-setdiff(drugs, 'Benzos')) %>%
  mutate(BenzosInt = if_else(Benzos == 'user', 1, 0))

save(trees_data, file = 'data/301_trees_data.RData')

### MODELING DECISION TREE #############################################################################################

classif_task <- makeClassifTask(id = 'tree',
                                data = select(trees_data[ix_trn, ],  -BenzosInt),
                                target = 'Benzos',
                                positive = 'user')

learner_rpart <- makeLearner('classif.rpart',predict.type = 'prob')

cv_scheme <- makeResampleDesc(method = 'RepCV',
                              folds = 4,
                              reps = 20,
                              stratify = TRUE,
                              predict = 'both')


### Hyperparameter search space ----------------------------------------------------------------------------------------

parameters_set <- makeParamSet(
  makeIntegerParam("minsplit", lower = 30, upper = 150),
  makeNumericParam("cp", lower = 0.0001, upper = 0.01)
)

ctrl <- makeTuneControlRandom(maxit = 30)


### Defining measures --------------------------------------------------------------------------------------------------

auc_trn <- setAggregation(auc, train.mean)
auc_sd <- setAggregation(auc, test.sd)


### Training with choosen hyparparameters ------------------------------------------------------------------------------

set.seed(42)

tune_results <- tuneParams(learner = learner_rpart,
           task = classif_task,
           resampling = cv_scheme,
           measures = list(auc_trn, auc, auc_sd),
           par.set = parameters_set,
           control = ctrl)


df_tune_results <- data.frame(tune_results$opt.path) %>%
  mutate(diff_auc = auc.train.mean - auc.test.mean) %>%
  arrange(diff_auc)


m4_rpart <- rpart(Benzos ~. -BenzosInt,
                  data = trees_data, 
                  subset = ix_trn,
                  control = rpart.control(minsplit = 149, cp = 0.0086))

rpart.plot(m4_rpart)

save(m4_rpart, file = 'results/301_m4_rpart.RData')

### MEASURING PERFORMANCE ##############################################################################################

pred_m4 <- data.frame(probs = predict(m4_rpart, newdata = trees_data, type = 'prob'),
                      category = predict(m4_rpart, newdata = trees_data,type = 'class'))

auc_rpart_trn <- AUC(pred_m4$probs.user[ix_trn], trees_data[ix_trn, 'BenzosInt'])
auc_rpart_tst <- AUC(pred_m4$probs.user[-ix_trn], trees_data[-ix_trn, 'BenzosInt'])

cm_m4_rpart <- confusionMatrix(pred_m4$category[-ix_trn], trees_data[-ix_trn, 'Benzos'], positive = 'user')

perf_rpart <- data.frame(model = 'decision tree',
                       auc_train = auc_rpart_trn,
                       auc_test = auc_rpart_tst,
                       auc_test_sd = data.frame(tune_results$opt.path)['auc.test.sd'][[1]][1],
                       precision_test = cm_m4_rpart$byClass['Precision'],
                       recall_test = cm_m4_rpart$byClass['Recall'])

save(perf_rpart, cm_m4_rpart, file = 'tmp/301_rpart_performace.RData')

rm(list = ls())

