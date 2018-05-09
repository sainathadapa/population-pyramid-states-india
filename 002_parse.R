library(magrittr)
library(dplyr)
library(stringr)
library(rlist)

downloaded_paths <- list.files('data/', full.names = TRUE)

parse_data <- function(x) {
  tmp <- readxl::read_excel(x)
  
  tmp <- tmp %>% 
    filter(str_detect(`Area Name`, '^State'))
  
  tmp <- tmp[,-((ncol(tmp)-11):1)]
  
  names(tmp) <- c("state", "age", "total_persons", "total_males", "total_females", 
                  "rural_persons", "rural_males", "rural_females", "urban_persons", 
                  "urban_males", "urban_females")
  
  tmp
}

all_states_data <- downloaded_paths %>%
  lapply(parse_data) %>% 
  do.call(what = bind_rows)

all_states_data$state <- all_states_data$state %>%
  str_replace_all('State - (.*) \\(.*\\)', '\\1') %>%
  tolower() %>%
  tools::toTitleCase()

all_states_data$state[all_states_data$state == "Nct of Delhi"] <- "NCT of Delhi"

write.csv(all_states_data, file = 'all_states_data.csv', row.names = FALSE)
