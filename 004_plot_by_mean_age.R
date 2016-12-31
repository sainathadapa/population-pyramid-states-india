library(ggplot2)
library(magrittr)
library(dplyr)
library(stringr)
library(tidyr)
library(svglite)
library(scales)
library(e1071)  

base_data <- readr::read_csv('all_states_data.csv')

# remove 'INDIA' data
base_data <- base_data %>% filter(!(state %in% 'INDIA'))

base_data %>% 
  filter(!(age %in% c('Age not stated', 'All ages'))) %>% 
  mutate(age = ifelse(age %in% '100+', 100, age)) %>% 
  mutate(age = age %>% as.numeric) %>% 
  mutate(age = cut(age,
                   breaks = seq(from = 0, to = 100, by = 5),
                   include.lowest = TRUE)) %>% 
  select(state, age, total_males, total_persons) %>% 
  group_by(state, age) %>% 
  summarise(total_males = sum(total_males),
            total_persons = sum(total_persons)) %>% 
  ungroup %>% 
  mutate(total_females = total_persons - total_males) -> 
  summarised_data

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
  theme_bw() +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0),
        plot.title = element_text(hjust = 0.5)) +
  ylab('% of Total population') +
  xlab('Age') + 
  scale_y_continuous(labels = function(breaks) breaks %>% multiply_by(100) %>% round(digits = 0) %>% paste0('%')) +
  scale_x_discrete(labels = NULL)

png(filename = 'temp.png', width = 1600, height = 900)
print(gg)
dev.off()

svglite('temp2.svg')
print(gg)
dev.off()

p <- ggplot(toplot, aes(x = age, y = total_persons, frame = rank)) + 
  geom_bar(stat = 'identity', position = "identity", fill = '#66BB6A') +
  coord_cartesian(expand = FALSE, ylim = c(0, max(toplot$total_persons))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0),
        plot.title = element_text(hjust = 0.5)) +
  ylab('% of Total population') +
  xlab('Age') + 
  geom_label(aes(y = 0.16, x = 20, label = state), vjust = "inward", hjust = "inward", color = '#66BB6A', label.size = 0, size = 7) +
  scale_y_continuous(labels = function(breaks) breaks %>% multiply_by(100) %>% round(digits = 0) %>% paste0('%'))

gganimate(p, "output.html", cumulative = TRUE, title_frame = FALSE)

