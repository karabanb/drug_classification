
### LIBRARIES ##########################################################################################################

library(tidyverse)
library(DALEX)
library(auditor)


### LOADING DATA #######################################################################################################

load('data/300_glm_data.RData')
load('data/301_trees_data.RData')

load('tmp/300_train_index.RData')
load('tmp/300_glm_performace.RData')
load('tmp/301_rpart_performace.RData')
load('tmp/302_rf_performace.Rdata')

load('results/300_m3_glm.RData')
load('results/301_m4_rpart.RData')
load('results/302_m6_rf.RData')



# Making Expaliners ----------------------------------------------------------------------------------------------------

exp_glm <- explain(model = m3_glm, 
                   data = select(glm_data[-ix_trn,], -Benzos, -BenzosInt),
                   y = glm_data$BenzosInt[-ix_trn], 
                   label = 'logistic regresion',
                   type = 'classification')


exp_rpart <- explain(model = m4_rpart,
                     data = select(trees_data[-ix_trn,], -Benzos),
                     y = trees_data[-ix_trn, 'BenzosInt'],
                     label = 'decsion tree',
                     type = 'classification')


exp_rf <- explain(model = m6_rf,
                  data = select(trees_data[-ix_trn,], -Benzos),
                  y = trees_data$BenzosInt[-ix_trn],
                  label = 'random forest',
                  type = 'classification')

# Measuring Preformance ------------------------------------------------------------------------------------------------


eval_glm <- model_evaluation(exp_glm)
eval_rpart <- model_evaluation(exp_rpart)
eval_rf <- model_evaluation(exp_rf)

plot(eval_glm, eval_rpart, eval_rf, type = 'roc')

results_df <- bind_rows(perf_glm, perf_rpart, perf_rf)
save(results_df, file = 'results/400_performance_metrics.RData')

rm(list = ls())



