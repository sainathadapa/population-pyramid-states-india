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

male_perc_rank <- summarised_data %>% 
  group_by(state) %>% 
  summarise(male_perc = sum(total_males)/sum(total_persons)) %>% 
  ungroup %>% 
  arrange(male_perc)

library(gganimate)

state_names <- male_perc_rank$state %>% 
  sapply(. %>% 
           tolower %>% 
           gsub(x = .,
                pattern = "\\b([[:alpha:]])([[:alpha:]]+)",
                replacement = "\\U\\1\\L\\2",
                perl = TRUE)) %>% 
  ifelse(. == 'Nct Of Delhi', 'Delhi', .) %>% 
  unname

state_names <- state_names %>% paste0('\n', "% Male: ", male_perc_rank$male_perc %>% multiply_by(100) %>% round(1))

toplot <- summarised_data %>% 
  mutate(state = factor(state, levels = male_perc_rank$state, ordered = TRUE, labels = state_names)) %>% 
  group_by(state, age) %>% 
  mutate(total_females = total_females/sum(total_persons)) %>% 
  ungroup %>% 
  mutate(female_diff_50 = (total_females - 0.5)) %>% 
  select(-total_persons, -total_males, -total_females) %>% 
  arrange(state, age)
  
gg1 <- ggplot(toplot) + 
  geom_bar(aes(x = age, y = female_diff_50, fill = (female_diff_50 > 0)), stat = 'identity', position = 'dodge') +
  facet_wrap(~state) +
  coord_cartesian(expand = FALSE) +
  theme_bw(base_size = 8) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0),
        plot.title = element_text(hjust = 0.5)) +
  ylab('(Females% - 50%) in age group') +
  xlab('Age') + 
  scale_y_continuous(labels = function(breaks) breaks %>% multiply_by(100) %>% round(digits = 0) %>% paste0('%')) +
  scale_x_discrete(breaks = levels(toplot$age)[seq(from = 1, to = 21, by = 2)],
                   labels = levels(toplot$age)[seq(from = 1, to = 21, by = 2)]) +
  scale_fill_manual(values = c('FALSE' = '#7986CB', 'TRUE' = '#FFB74D'),
                    labels = c('FALSE' = '% Males > % Females', 'TRUE' = '% Males < % Females'),
                    name = '') +
  theme(legend.position = c(0.92, 0.06),
        legend.direction = 'vertical') +
  ggtitle('States & UTs sorted by percentage of males among total population')

svglite('states-sorted-sex-diff.svg')
print(gg1)
dev.off()

