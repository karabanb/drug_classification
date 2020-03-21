

### LIBRARIES ##########################################################################################################

library(tidyverse)
library(funModeling)
library(inspectdf)

### LOADING DATA #######################################################################################################

load('data/100_preprocessed_data.RData')


# helper vectors

drugs <- c('Amphet', 'Amyl', 'Benzos', 'Caffeine', 'Cannabis', 'Chocloate', 'Coke', 'Crack', 'Extasy', 'Heroine',
  'Ketamine', 'LegalHighs', 'LSD', 'Meth', 'Mushrooms', 'Nicotine', 'Semeron', 'VSA')


### Exploration od Catagorical Variables ###############################################################################

# Inspecting imbalancess of possible target variables -----------------------------------------------------------------

usage_freq <- inspect_cat(preprocessed_data[, drugs])

usage_freq <- usage_freq %>% 
  unnest_legacy() %>%
  mutate(drug = as.factor(col_name))

usage_freq$drug <- fct_reorder2(usage_freq$drug, usage_freq$value, usage_freq$prop)

ggplot(usage_freq, aes(x = drug, y = prop, fill = value)) + 
  geom_bar(stat = 'identity', position = 'fill', alpha = 0.7)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        legend.title = element_blank()) + 
  geom_text(aes(label = round(prop, 2)),
            position = position_stack(vjust = 0.5),
            color="black",
            size=3) +
  xlab('Drug') +
  ylab('Fraction') +
  coord_flip() +
  ggtitle('Fraction of Users/Non-users in Each Category of Drug') 

# As we can see on this chart the most balanced targed variable is 'Benzos', so I decided to choose this one 
# as target variable to modeling.


# Distributions of Other Categorical Variables -------------------------------------------------------------------------

cat_vars <- c('Age', 'Gender', 'Education', 'Country', 'Ethnicity')
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


ggplot(categ_fractions, aes(x = prop, y = level)) + 
  facet_wrap(.~variable, scale = 'free_y') +
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_text(aes(label = round(prop, 2)),
            color = "black",
            nudge_x = 0.2,
            size = 3) +
  theme_bw()


### Merging rare factor levels (if they occure in less than 3%)  -------------------------------------------------------

preprocessed_data <- preprocessed_data %>%
  mutate_at(cat_vars, fct_lump_prop, prop = 0.03) %>% 
  mutate(Age = fct_collapse(preprocessed_data$Age, '55+' = c('55-64', '65 +')))


### Fraction of Target's categories in categorical variables -----------------------------------------------------------

categ_fractions <- preprocessed_data %>%
  pivot_longer(cols = cat_vars,
               names_to = 'variable',
               values_to = 'level') %>% 
  select(variable, level, Benzos) %>%
  group_by(variable, level) %>%
  summarise(n = n(), 
            prop = n()/n_obs,
            users = sum(Benzos=='user')/n,
            non_users = 1 - users) %>%
  pivot_longer(cols = c('users', 'non_users'),
               names_to = 'target',
               values_to = 'fraction')

categ_fractions$level <- fct_reorder2(categ_fractions$level,
                                      categ_fractions$target,
                                      categ_fractions$fraction,
                                      .fun = min
)


ggplot(categ_fractions, aes(x = fraction, y = level, fill = target)) +
  facet_wrap(.~variable, scale = 'free_y') +
  geom_bar(stat = 'identity', alpha = 0.5) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  geom_text(aes(label = round(fraction, 2)),
            color = "black",
            position = position_stack(vjust = 0.5),
            size = 3) +
  ggtitle('Distribution of Target in each Category of each Variable')
  

### Exploration od Continuus Variables ###############################################################################













  
  

  
