library(ggplot2)

library(geofacet)

ggplot(state_ranks, aes(variable, rank, fill = variable)) +
  geom_col() +
  coord_flip() +
  facet_geo(~ state) +
  theme_bw()

library(tidyverse)

# code, name, row, col
new_grid <- data.frame(code = c('a', 'b'), name = c('a', 'b'), row = c(1, 2), col = c(1, 1))

ggplot(tmp) +
  geom_point(aes(speed, dist)) +
  facet_geo(~state, grid = new_grid)

old_names <- sort(unique(summarised_data$state))
new_names <- sort(state_names)
state_map <- new_names
names(state_map) <- old_names

summarised_data$state <- unname(state_map[summarised_data$state])

summarised_data %>% 
  ggplot() +
  geom_bar(aes(x = age, y = total_males),  stat = 'identity') +
  facet_wrap(~state)

tmp <- c("Jammu & Kashmir",
         "Himachal Pradesh",
         "Punjab", "Haryana", "Uttarakhand",                             "Arunachal Pradesh",
         "Rajasthan", "Delhi", "Uttar Pradesh", "Bihar",                 "Meghalaya", "Assam", "Nagaland",
         "Gujarat", "Madhya Pradesh", "Jharkhand", "West Bengal","Sikkim","Tripura", "Mizoram", "Manipur",
         "Maharashtra", "Telangana","Chhattisgarh", "Odisha",
         "Goa", "Karnataka", "Andhra Pradesh",
         "Kerala", "Tamil Nadu")

new_grid <- data.frame(code = tmp,
                       name = tmp,
                       row = c(1, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 8, 8),
                       col = c(1, 1, 1, 2, 3, 4, 1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 1, 2, 3, 1, 2))

summarised_data %>% 
  mutate(state = ifelse(state %in% 'Chandigarh', 'Telangana', state)) %>% 
  filter(state %in% tmp) %>% 
  ggplot() +
  geom_bar(aes(x = age, y = total_males),  stat = 'identity') +
  facet_geo(~state, grid = new_grid)
