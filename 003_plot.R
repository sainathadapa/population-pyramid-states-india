library(ggplot2)
library(magrittr)
library(dplyr)
library(stringr)
library(tidyr)

base_data <- readr::read_csv('all_states_data.csv')

# remove 'INDIA' data
base_data <- base_data %>% filter(!(state %in% 'INDIA'))

# does every state have the same number of rows
base_data %>% 
  group_by(state) %>% 
  summarise(n = n()) %>% 
  ungroup %>% 
  select(n) %>% 
  distinct
# yes
  
# is the age column values identical for all states
base_data %>% 
  select(state, age) %>% 
  arrange(state, age) %>% 
  nest(-state) %>% 
  extract2('data') %>% 
  unique
# yes

# check if the % of 'Age not stated' is high in any state
base_data %>% 
  select(state, age, total_persons) %>% 
  filter(age %in% c('Age not stated', 'All ages')) %>% 
  spread(key = 'age', value = 'total_persons') %>% 
  mutate(perc = `Age not stated` * 100 / `All ages`) -> 
  toplot
  
ggplot(toplot) +
  geom_bar(aes(x = state, y = perc), stat = 'identity') +
  coord_flip() +
  xlab('State') +
  ylab('% of "Age not stated"')
# It is within 1% for all states. Should be ok

one_state_data <- base_data %>% 
  filter(state %in% 'ANDHRA PRADESH') %>% 
  filter(!(age %in% c('Age not stated', 'All ages'))) %>% 
  mutate(age = age %>% as.numeric) %>% 
  mutate(age = ifelse(is.na(age), 100, age))

# people round off to the nearest 5 multiple if they don't know the exact value
one_state_data$age <- cut(one_state_data$age, breaks = seq(from = 0, to = 100, by = 5), include.lowest = TRUE)

one_state_data %>% 
  group_by(age) %>% 
  summarise(total_males = sum(total_males),
            total_females = sum(total_females)) %>% 
  ungroup ->
  toplot

gg <- ggplot(toplot) +
  geom_bar(aes(x = age, y = -total_males), stat = 'identity', fill = '#7986CB', color = 'black', alpha = 0.8, width = 1) +
  geom_bar(aes(x = age, y = total_females), stat = 'identity', fill = '#FFB74D', color = 'black', alpha = 0.8, width = 1) +
  coord_flip() +
  geom_hline(aes(yintercept = 0)) +
  theme_bw() +
  ylab('Number of persons') +
  xlab('Age')

png(filename = 'ap1.png', width = 450, height = 750)
plot(gg)
dev.off()

toplot %>% 
  gather(key = 'key', value = 'value', -age) ->
  toplot

gg <- ggplot(toplot) +
  geom_bar(aes(x = age, y = value, fill = key), stat = 'identity', position = 'dodge') +
  scale_y_continuous(label = scales::comma) +
  scale_fill_manual(values = c(total_males = '#7986CB', total_females = '#FFB74D')) +
  coord_cartesian(expand = FALSE)

png(filename = 'ap2.png', width = 1500, height = 900)
plot(gg)
dev.off()

toplot %>% 
  group_by(age) %>% 
  mutate(percent = value / sum(value)) %>% 
  ungroup ->
  toplot

gg <- ggplot(toplot) +
  geom_bar(aes(x = age, y = percent, fill = key), stat = 'identity') +
  scale_y_continuous(label = scales::comma) +
  scale_fill_manual(values = c(total_males = '#7986CB', total_females = '#FFB74D')) +
  coord_cartesian(ylim = c(0, 1), expand = FALSE)

png(filename = 'ap3.png', width = 1500, height = 900)
plot(gg)
dev.off()
