library(magrittr)
library(dplyr)
library(stringr)
# I used the package at this state : https://github.com/steadyfish/ogdindiar/pull/18
library(ogdindiar)

# Get the list of all datasets in this catalog
all_datasets <-  get_datasets_from_a_catalog(
  catalog_link = 'https://data.gov.in/catalog/population-single-year-age-residence-and-sex-india-and-states',
  limit_dataset_pages = Inf,
  limit_datasets = Inf)

all_datasets <- all_datasets %>% distinct %>% arrange(name)

# Extracting the name of the state and year
all_datasets <- all_datasets %>% 
  mutate(year = str_extract(name, '[0-9]+')) %>% 
  mutate(state = str_replace_all(name, '.*[0-9]+ - (.*)', '\\1')) %>% 
  mutate(name = str_replace_all(name, '(.*)[0-9]{4}.*', '\\1') %>% str_trim) %>%
  select(name, state, year, excel)

req_datasets <- all_datasets %>% 
  filter(name %in% "Population in single year age by Residence and Sex,") %>% 
  select(-name) %>% 
  arrange(state, year)

# Selecting the latest dataset for each state
req_datasets <- req_datasets %>% 
  group_by(state) %>% 
  filter(year == max(year)) %>% 
  ungroup


write.csv(req_datasets, file = 'dataset_list.csv', row.names = FALSE)
