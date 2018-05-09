library(ggplot2)
library(magrittr)
library(dplyr)
library(stringr)
library(tidyr)
library(svglite)
library(scales)

base_data <- readr::read_csv('all_states_data.csv')

# Q: does every state have the same number of rows?
base_data %>% 
  group_by(state) %>% 
  summarise(n = n()) %>% 
  ungroup %>% 
  select(n) %>% 
  distinct
# A: yes

# Q: is the age column values identical for all states
base_data %>% 
  select(state, age) %>% 
  arrange(state, age) %>% 
  nest(-state) %>% 
  extract2('data') %>% 
  unique
# A: yes

# Q: check if the % of 'Age not stated' is high in any state
base_data %>% 
  select(state, age, total_persons) %>% 
  filter(age %in% c('Age not stated', 'All ages')) %>% 
  spread(key = 'age', value = 'total_persons') %>% 
  mutate(perc = `Age not stated` * 100 / `All ages`) -> 
  age_not_stated

ggplot(age_not_stated) +
  geom_bar(aes(x = state, y = perc), stat = 'identity') +
  coord_flip() +
  xlab('State') +
  ylab('% of "Age not stated"')
# A: It is within 1% for all states. Should be ok.
# suprising to see Andhra pradesh to be one of the top states with age not stated :O

base_data %>% 
  filter(!(age %in% c('Age not stated', 'All ages'))) %>% 
  mutate(age = ifelse(age %in% '100+', 101, age)) %>% 
  mutate(age = age %>% as.numeric) %>% 
  mutate(age = cut(age,
                   breaks = seq(from = 0, to = 105, by = 5),
                   include.lowest = TRUE,
                   right = FALSE)) %>% 
  select(state, age, total_males, total_persons) %>% 
  group_by(state, age) %>% 
  summarise(total_males = sum(total_males),
            total_persons = sum(total_persons)) %>% 
  ungroup %>% 
  mutate(total_females = total_persons - total_males) -> 
  summarised_data

age_levels <- summarised_data$age %>% levels
age_labels <- summarised_data$age %>%
  levels %>% 
  str_replace_all(fixed('['), '') %>% 
  str_replace_all(fixed(')'), '') %>% 
  str_replace_all(fixed(']'), '') %>% 
  str_split_fixed(',', 2) %>% 
  as_data_frame %>% 
  mutate(V1 = as.numeric(V1),
         V2 = as.numeric(V2) - 1) %>% 
  mutate(ans = paste0(V1, '-' , V2)) %>% 
  extract2('ans')
age_labels[21] <- '100+'
summarised_data$age <- factor(as.character(summarised_data$age), age_levels, age_labels, ordered = TRUE)

# plotting one state at a time
for (one_state in sort(unique(summarised_data$state))) {
  message(one_state)
  
  state_name <- one_state %>% 
    tolower %>% 
    gsub(x = .,
         pattern = "\\b([[:alpha:]])([[:alpha:]]+)",
         replacement = "\\U\\1\\L\\2",
         perl = TRUE)
  
  one_state_data_long <- summarised_data %>% 
    filter(state %in% one_state) %>% 
    select(-state, -total_persons) %>% 
    gather(key = 'key', value = 'value', -age)
  
  gg1 <- one_state_data_long %>% 
    mutate(value = ifelse(key %in% 'total_males', -value, value)) %>% 
    ggplot() +
    geom_bar(aes(x = age, y = value, fill = key),  stat = 'identity') +
    scale_fill_manual(values = c(total_males = '#7986CB', total_females = '#FFB74D'),
                      labels = c(total_males = 'Male',    total_females = 'Female'),
                      name = '') +
    coord_flip() +
    scale_y_continuous(labels = . %>% abs %>% comma) +
    theme_bw(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.8, 0.9),
          legend.direction = 'horizontal') +
    ylab('Population') +
    xlab('Age') +
    ggtitle(state_name)
  
  svglite(paste0('plots/', one_state, '-1.svg'))
  plot(gg1)
  dev.off()
  
  gg2 <- ggplot(one_state_data_long) +
    geom_bar(aes(x = age, y = value, fill = key), stat = 'identity', position = 'dodge') +
    scale_y_continuous(label = scales::comma) +
    scale_fill_manual(values = c(total_males = '#7986CB', total_females = '#FFB74D'),
                      labels = c(total_males = 'Male',    total_females = 'Female'),
                      name = '') +
    coord_cartesian(expand = FALSE) +
    theme_bw(base_size = 15) +
    theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0),
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0.8, 0.9),
          legend.direction = 'horizontal') +
    ylab('Population') +
    xlab('Age') +
    ggtitle(state_name)
  
  
  svglite(paste0('plots/', one_state, '-2.svg'))
  plot(gg2)
  dev.off()
  
  one_state_data_long %>% 
    group_by(age) %>% 
    mutate(percent = value / sum(value)) %>% 
    ungroup ->
    one_state_data_long
  
  gg3 <- ggplot(one_state_data_long) +
    geom_bar(aes(x = age, y = percent, fill = key), stat = 'identity') +
    geom_hline(yintercept = 0.5, alpha = 0.5, linetype = 2) +
    scale_fill_manual(values = c(total_males = '#7986CB', total_females = '#FFB74D'),
                      labels = c(total_males = 'Male',    total_females = 'Female'),
                      name = '') +
    coord_cartesian(ylim = c(0, 1), expand = FALSE) +
    theme_bw(base_size = 15) +
    theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0),
          plot.title = element_text(hjust = 0.5),
          legend.position = 'bottom',
          legend.direction = 'horizontal') +
    ylab('Number of Males / Total population') +
    xlab('Age') +
    ggtitle(state_name)
  
  svglite(paste0('plots/', one_state, '-3.svg'))
  plot(gg3)
  dev.off()
}
