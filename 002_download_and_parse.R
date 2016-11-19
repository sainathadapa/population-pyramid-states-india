library(ogdindiar)
library(magrittr)
library(dplyr)
library(stringr)
library(rlist)

datasets <- readr::read_csv('dataset_list.csv')

datasets$downloaded_paths <- lapply(datasets$excel, download_dataset) %>% 
  list.mapv(filepath)

parse_data <- function(x) {
  tmp <- readxl::read_excel(x)
  
  tmp <- tmp %>% 
    filter(str_detect(`Area Name`, '^State'))
  
  tmp <- tmp[,-((ncol(tmp)-10):1)]
  
  names(tmp) <- c("age", "total_persons", "total_males", "total_females", 
                  "rural_persons", "rural_males", "rural_females", "urban_persons", 
                  "urban_males", "urban_females")
  
  tmp
}

all_states_data <- datasets$downloaded_paths %>%
  lapply(parse_data) %>% 
  setNames(datasets$state) %>% 
  list.map(data.frame(state = .name, .)) %>% 
  do.call(what = bind_rows)

write.csv(all_states_data, file = 'all_states_data.csv', row.names = FALSE)
