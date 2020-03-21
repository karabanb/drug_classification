
tmp <- tmp %>% 
  mutate(V17 = if_else(V17 == 'user', 0, 1))

m1 <- glm(V17 ~ Age + 
            Gender + 
            Education + 
            Country + 
            Ethnicity +
            V7 +
            V8 +
            V9 +
            V10 +
            V11 +
            V12 +
            V13,
          data = tmp[ix_tran,],
          family = 'binomial')

m1_aic <- step(m1, direction = 'both')

car::vif(m1_aic)

pred_m1_aic_trn <- predict(m1_aic, newdata = tmp[ix_tran,], type = 'response')
pred_m1_aic_tst <- predict(m1_aic, newdata = tmp[-ix_tran,], type = 'response')

Metrics::auc(tmp[ix_tran, 'V17'], pred_m1_aic_trn)
Metrics::auc(tmp[-ix_tran, 'V17'], pred_m1_aic_tst)

