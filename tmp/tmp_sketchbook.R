
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



DataExplorer::plot_bar(preprocessed_data)

categ_plot <- DataExplorer::plot_bar(preprocessed_data[, cat_vars])

categ_plot_2 <-inspectdf::inspect_cat(preprocessed_data[, cat_vars])
show_plot(categ_plot_2)

funModeling::freq(preprocessed_data[, cat_vars])


categories <- list()
categ_analysis(preprocessed_data, target = 'Benzos', input = 'Ethnicity')

z<- sapply(preprocessed_data, 
           function(x){categ_analysis(data = preprocessed_data, target = 'Benzos', input = x)})

n_obs <- nrow(preprocessed_data)

categ_fractions <- preprocessed_data %>%
  pivot_longer(cols = cat_vars,
               names_to = 'variable',
               values_to = 'level') %>% 
  select(variable, level, Benzos) %>%
  group_by(variable, level) %>%
  summarise(n = n(), 
            prop = n()/n_obs,
            users = sum(Benzos=='user')/n,
            non_users = 1 - users)
 
 

funModeling::cross_plot(preprocessed_data_tmp[, c(cat_vars, 'Benzos')],
                        target = 'Benzos',
                        plot_type = 'percentual',
                        path_out = 'results')



categ_fractions_long <- categ_fractions %>% 
  pivot_longer(cols = c('users', 'non_users'),
               names_to = 'target',
               values_to = 'fraction')

ggplot(categ_fractions, aes(x = prop, y = level)) + 
  facet_wrap(.~variable, scale = 'free_y') +
  geom_bar(stat = 'identity', alpha = 0.5) +
  theme_bw()


ggplot(categ_fractions_long, aes(x = fraction, y = level, fill = target)) +
  facet_wrap(.~variable, scale = 'free_y') +
  geom_bar(stat = 'identity')



numeric_vars <- preprocessed_data %>% 
  select_if(is.numeric) %>%
  colnames()

numerical_longer <- preprocessed_data %>% 
  pivot_longer(cols = numeric_vars,
               names_to = 'variable',
               values_to = 'score'
               ) %>% 
  select(variable, score, Benzos)

ggplot(numerical_longer, aes(score)) +
  facet_wrap(.~variable) +
  geom_density() +
  theme_bw()


ggplot(numerical_longer, aes(score, fill = Benzos)) +
  facet_wrap(~variable) +
  geom_boxplot(alpha = 0.3) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        axis.text.y = element_blank()) +
  ggtitle('Boxplots of  ')



