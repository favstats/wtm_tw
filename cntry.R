

library(tidyverse)

custom <- T

coded <- read_csv("data/FacebookAdLibraryReport_2024-01-05_TW_last_30_days_advertisers_coded.csv")


party_dat <- coded %>% 
  janitor::clean_names() %>%
  filter(party != "Non-Party") %>% 
  filter(party != "Unaffiliated") %>% 
  mutate(page_id = as.character(page_id)) 

color_dat <- party_dat %>% 
  count(party, sort = T) %>% 
  mutate(colors = case_when(
    str_detect(party, "KMT") ~ "#000096",
    str_detect(party, "DPP") ~ "#009B00",
    str_detect(party, "Statebuilding") ~ "#A73F24",
    str_detect(party, "New Power") ~ "#FBBE01",
    str_detect(party, "Labor") ~ "#FF0000",
    str_detect(party, "First Party") ~ "#FF6310",
    str_detect(party, "Taiwan People's Party") ~ "#28C7C7"
  )) %>% 
  select(-n)

saveRDS(color_dat, "data/color_dat.rds")

all_dat <- readRDS("data/all_dat.rds")

all_dat <- all_dat %>% 
  select(-party) %>% 
  left_join(party_dat %>% distinct(page_id, party)) 

saveRDS(all_dat, "data/all_dat.rds")
