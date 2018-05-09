library(ggplot2)
library(magrittr)
library(dplyr)
library(stringr)
library(tidyr)
library(svglite)
library(scales)
library(e1071)  

base_data <- readr::read_csv('all_states_data.csv')

base_data %>% 
  filter(!(age %in% c('Age not stated', 'All ages'))) %>% 
  mutate(age = ifelse(age %in% '100+', 101, age)) %>% 
  mutate(age = age %>% as.numeric) %>% 
  mutate(age = cut(age,
                   breaks = seq(from = 0, to = 105, by = 5),
                   include.lowest = TRUE, right = FALSE)) %>% 
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

mean_rank <- base_data %>% 
  filter(!(age %in% c('Age not stated', 'All ages'))) %>% 
  mutate(age = ifelse(age %in% '100+', 100, age)) %>% 
  mutate(age = age %>% as.numeric) %>% 
  group_by(state) %>% 
  summarise(mean_age = sum(age*total_persons)/sum(total_persons)) %>% 
  ungroup %>% 
  arrange(mean_age)

library(gganimate)

state_names <- mean_rank$state %>% 
  sapply(. %>% 
           tolower %>% 
           gsub(x = .,
                pattern = "\\b([[:alpha:]])([[:alpha:]]+)",
                replacement = "\\U\\1\\L\\2",
                perl = TRUE)) %>% 
  ifelse(. == 'Nct Of Delhi', 'Delhi', .) %>% 
  unname

state_names <- state_names %>% paste0('\n', "Mean Age: ", mean_rank$mean_age %>% round(1))

toplot <- summarised_data %>% 
  select(state, age, total_persons) %>% 
  mutate(state = factor(state, levels = mean_rank$state, ordered = TRUE, labels = state_names)) %>% 
  mutate(rank = as.numeric(state)) %>% 
  group_by(state) %>% 
  mutate(total_persons = total_persons/sum(total_persons)) %>% 
  ungroup %>% 
  arrange(state, age)

gg <- ggplot(toplot) + 
  geom_bar(aes(x = age, y = total_persons), stat = 'identity') +
  facet_wrap(~state) +
  coord_cartesian(expand = FALSE) +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0),
        plot.title = element_text(hjust = 0.5)) +
  ylab('% of Total population') +
  xlab('Age') + 
  scale_y_continuous(labels = function(breaks) breaks %>% multiply_by(100) %>% round(digits = 0) %>% paste0('%')) +
  scale_x_discrete(breaks = levels(toplot$age)[seq(from = 1, to = 21, by = 2)],
                   labels = levels(toplot$age)[seq(from = 1, to = 21, by = 2)]) +
  ggtitle('States & UTs sorted by mean age')

svglite('states-sorted-mean-age.svg')
print(gg)
dev.off()

toplot <- summarised_data %>% 
  mutate(state = factor(state, levels = mean_rank$state, ordered = TRUE, labels = state_names)) %>% 
  group_by(state) %>% 
  mutate(total_males   = total_males/sum(total_persons),
         total_females = total_females/sum(total_persons)) %>% 
  ungroup %>% 
  select(-total_persons) %>% 
  gather(key = 'var', value = 'val', -state, -age) %>% 
  arrange(state, age)

gg <- toplot %>% 
  mutate(val = ifelse(var %in% 'total_males', -val, val)) %>% 
  ggplot() +
  geom_bar(aes(x = age, y = val, fill = var),  stat = 'identity', width = 1) +
  scale_fill_manual(values = c(total_males = '#7986CB', total_females = '#FFB74D'),
                    labels = c(total_males = 'Male',    total_females = 'Female'),
                    name = '') +
  coord_flip() +
  facet_wrap(~state) +
  scale_y_continuous(labels = function(breaks) breaks %>% multiply_by(100) %>% abs %>% round(digits = 0) %>% paste0('%')) +
  scale_x_discrete(breaks = levels(toplot$age)[seq(from = 1, to = 21, by = 4)],
                   labels = levels(toplot$age)[seq(from = 1, to = 21, by = 4)]) +
  theme_bw(base_size = 9) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.9, 0.08),
        legend.direction = 'vertical') +
  ylab('% Population') +
  xlab('Age') +
  ggtitle('States & UTs sorted by mean age')

svglite('states-sorted-mean-age2.svg')
print(gg)
dev.off()
