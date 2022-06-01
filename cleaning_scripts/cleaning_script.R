library(here)
library(janitor)
library(tidyverse)

# loading the data

animal_complaints <- read_csv(here("raw_data/animal_complaints.csv")) %>% 
  clean_names()

animal_complaints

animal_outcomes <-  read_csv(here("raw_data/animal_outcomes.csv")) %>% 
  clean_names()

animal_outcomes

brisbane_complaints <- read_csv(here("raw_data/brisbane_complaints.csv")) %>% 
  clean_names()

brisbane_complaints

# exploring the data

# checking NAs

colSums(is.na(animal_complaints)) # no NAs at all here

colSums(is.na(animal_outcomes)) # a few missing values in the regions columns, nothing major

colSums(is.na(brisbane_complaints)) # the responsible_office column is empty, there's quite a few NAs in the category column

# I can get two different datasets from animal_complaints, one for generating visuals on the trend through time and one for total

animal_complaints_clean <- animal_complaints %>%  # for trend through time, dropping electoral_division, don't see an use for it
  select(- electoral_division)

animal_complaints_tot_per_suburb <- animal_complaints_clean %>% # for totals 
  group_by(animal_type, complaint_type, suburb) %>% 
  summarise(total = n())

# for animal_outcomes I will do a `pivot_longer()` moving all the states codes in a single column then I'll rename them to their full name so they 
# are easy to read. I will remove the total column as I don't really see an use for it.

animal_outcomes_clean <- animal_outcomes %>%
  pivot_longer(4:11, names_to = "state", values_to = "tot_outcomes") %>% 
  mutate(state = str_replace_all(state, "act", "Australian Capital Territory"),
         state = str_replace_all(state, "nsw", "New South Wales"),
         state = str_replace_all(state, "nt", "Northern Territory"),
         state = str_replace_all(state, "nt", "Northern Territory"),
         state = str_replace_all(state, "qld", "Queensland"),
         state = str_replace_all(state, "sa", "South Australia"),
         state = str_replace_all(state, "tas", "Tasmania"),
         state = str_replace_all(state, "vic", "Victoria"),
         state = str_replace_all(state, "wa", "Western Australia")) %>% 
  select(-total) 

brisbane_complaints_clean %>% # checking rows in data_range column 
  group_by(date_range) %>% 
  summarise(n = n())

brisbane_complaints_clean <- brisbane_complaints %>% # dropping columns I don't need and renaming the dates to a more readble standard
  select(-nature, -city, -responsible_office) %>% 
  mutate(category = coalesce(category, median(category, na.rm = T)),
         date_range = str_remove_all(date_range, ".csv"),
         date_range = str_replace_all(date_range, "1st-quarter-2016-17", 
                                     "january march 2016"),
         date_range = str_remove_all(
           date_range, "cars-srsa-open-data-animal-related-complaints-"),
         date_range = str_replace_all(date_range, "-to-", " "),
         date_range = str_replace_all(date_range, "-", " "))

# writing clean data

write_csv(animal_complaints_clean, file = here(
               "clean_data/animal_complaints_clean.csv"))

write_csv(animal_complaints_tot_per_suburb, file = here(
               "clean_data/animal_complaints_tot_per_suburb.csv"))

write_csv(animal_outcomes_clean, file = here(
               "clean_data/animal_outcomes_clean.csv"))

write_csv(brisbane_complaints_clean, file = here(
               "clean_data/brisbane_complaints_clean.csv"))