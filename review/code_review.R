# AY Code Review for Misconduct Data

# Loading Libraries
library(tidyverse)
library(lubridate)
library(janitor)
library(zoo)
library(scales)

# Reading in data
# AY: For future reference, the best practice for reading files is to use the here package to specify the file paths of your data. This makes it easier to 
# collaborate on scripts with multiple users. For example, the original code as it was written in the misconduct_data_cleaning script will not run on my 
# machine, but as I've tweaked it below, it *should* run equivalently for both of us.

allegations <- read_csv(here::here("original_data/misconduct_original_data/LLEAD_data/data_allegation.csv"))
personnel <- read_csv(here::here("original_data/misconduct_original_data/LLEAD_data/data_personnel.csv"))
agency_locations <- read_csv(here::here("original_data/misconduct_original_data/LLEAD_data/data_agency-reference-list.csv"))
allegation_class <- read_csv(here::here("original_data/misconduct_original_data/classifiers/data_allegation_classification.csv"))
disposition_class <- read_csv(here::here("original_data/misconduct_original_data/classifiers/data_disposition_classification.csv"))
repercussion_class <- read_csv(here::here("original_data/misconduct_original_data/classifiers/data_repercussion_classification.csv"))

# Renaming "agency_slug" as "agency" in the location data so that we can merge below
# AY: Tweaking this into a tidyverse format

agency_locations <- agency_locations %>%
  rename(agency = agency_slug)

# Getting full names of officers, merging with allegation, disposition, and repercussion categories, and replace empty values
# AY: Let's update this to use the join family of functions, rather than merge. the join functions are faster and preserve the original order of your data

misconduct <- personnel %>%
  unite(col='full_name', c('first_name', "middle_name", 'last_name') , sep = " ", na.rm = TRUE) %>%
  left_join(allegations, by = c("uid", "agency", "race", "sex")) %>%
  left_join(agency_locations, by = "agency") %>%
  left_join(allegation_class, by = "allegation") %>%
  left_join(disposition_class, by = "disposition") %>%
  left_join(repercussion_class, by = "action") %>%
  select(-c("allegation_categories", "allegation_categories_description", "disposition_categories", "disposition_categories_description", 
            "action_categories", "action_categories_description")) %>%
  mutate(allegation_classification = replace_na(allegation_classification, "No Allegation Reported"),
         disposition_classification = replace_na(disposition_classification, "No Disposition Reported"),
         action_classification = replace_na(action_classification, "No Repercussion Reported"),
         full_name = ifelse(full_name == "", "Unnamed Officer", full_name))

# Number of misconducts in the data
# AY: are you concerned at all about duplicates? 
n_misconduct <- nrow(misconduct)

# Number of police officers in the misconduct data
n_police <- length(unique(misconduct$uid))

# Number of police departments
n_departments <- length(unique(misconduct$agency_name))

# Representation of police departments in the data
department_count <- misconduct %>% 
  tabyl(agency_name)

# % of agencies that reported race
# AY: these are a little hard to read as written. Re-writing into a tidy format to make them more legible and adding a convenience function to reduce
# duplicated code

get_pct_var_reported <- function(df) {
  df %>%
  clean_names() %>%
  mutate(non_na_sum = rowSums(across(!c(agency_name, na))),
         var_reported = if_else(non_na_sum > 0, T, F)) %>%
  tabyl(var_reported)  
}

misconduct %>%
  tabyl(agency_name, race) %>%
  get_pct_var_reported()

# % of agencies that reported gender

misconduct %>%
  tabyl(agency_name, sex) %>%
  get_pct_var_reported()

# % of allegations that have reported repercussions

misconduct %>%
  tabyl(agency_name, action) %>%
  get_pct_var_reported()

# % of allegations that the officers name is included
# it appears that there are no rows without an officer's name reported?

# % of allegations that have reported dispositions

misconduct %>%
  tabyl(agency_name, disposition) %>%
  get_pct_var_reported()

# Incompleteness scores for each department
# AY: can you walk me through what your process is here? also introducing skimr package in case it's useful

skimr::skim(misconduct) %>%
  View()

incompleteness_score <- misconduct %>%
  mutate(score = rowSums(is.na(.))) %>%
  group_by(agency_name) %>%
  summarize(score = -mean(score)) %>%
  mutate(score = rescale(score)*100) %>%
  arrange(desc(score))

# Number of officers by race 
# AY: it looks like you had a few extra lines of code here that you didn't need

officer_race_count <- misconduct %>%
  distinct(uid, .keep_all = T) %>%
  tabyl(race)

# Number of officers by gender 
officer_sex_count <- misconduct %>%
  distinct(uid, .keep_all = T) %>%
  tabyl(sex)

# People with most allegations of misconduct per department
# AY: this should do the same calculation as your list, but in a single df instead

misconduct %>%
  distinct(uid, .keep_all = TRUE) %>%
  group_by(agency_name, full_name) %>%
  summarize(n = n()) %>%
  arrange(desc(n), .by_group = T) %>%
  View()

# Percent of officers who are male
# AY: altering this so that it's not hard-coded by location. we want to avoid that if possible

pct_officers_male <- officer_sex_count %>%
  filter(sex == "Male") %>%
  adorn_pct_formatting() %>%
  pull(valid_percent)

# Percent of officers who are black

pct_officers_black <- officer_race_count %>%
  filter(race == "Black") %>%
  adorn_pct_formatting() %>%
  pull(valid_percent)

# Percent of officers who are white

pct_officers_white <- officer_race_count %>%
  filter(race == "White") %>%
  adorn_pct_formatting() %>%
  pull(valid_percent)

# Average number of allegations per officer
average_allegations <- mean(tabyl(misconduct$uid)$n)

# Average number of allegations per sex
average_allegations_male <- mean(tabyl(filter(misconduct, sex == "Male")$uid)$n)
average_allegations_female <- mean(tabyl(filter(misconduct, sex == "Female")$uid)$n)
average_allegations_na <- mean(tabyl(filter(misconduct, is.na(sex))$uid)$n)

# Number of allegations in each category
allegation_distribution <- misconduct %>%
  tabyl(allegation_classification)

# Number of disposition in each category
disposition_distribution <- misconduct %>%
  tabyl(disposition_classification)

# Number of repercussions in each category
repercussion_distribution <- misconduct %>%
  tabyl(action_classification)

# Number of use of force allegations
# AY: again, trying to avoid hard coding

n_uof <- allegation_distribution %>%
  filter(allegation_classification == "Excessive Use of Force") %>%
  pull(n)

# Percent of allegations that were sustained

pct_sustained <- disposition_distribution %>%
  filter(disposition_classification == "Sustained") %>%
  adorn_pct_formatting() %>%
  pull(percent)

# Number of terminations

n_terminated <- repercussion_distribution %>%
  filter(action_classification == "Termination") %>%
  pull(n)

# Percent of allegations terminated
# AY: am not sure why you're filtering out the no repercussion reported allegations?
percent_terminated <- 100 * n_terminated/nrow(filter(misconduct, action_classification != "No Repercussion Reported"))

# Number of arrests
n_arrested <- repercussion_distribution[1,2]

# Percent arrested
percent_arrested <- 100 * n_arrested/nrow(filter(misconduct, action_classification != "No Repercussion Reported"))

# Percent of each disposition by allegation type
sustain_status_by_allegation_percent <- misconduct %>%
  tabyl(disposition_classification,allegation_classification) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2, affix_sign = FALSE)

# Number of sustained per allegation type
sustained_by_allegation <- misconduct %>%
  tabyl(allegation_classification, disposition_classification) %>%
  adorn_totals("col") %>%
  select(c("allegation_classification", "Sustained", "Total"))

# Distribution of repercussions by misconduct type
repercussion_by_allegation <- misconduct %>%
  tabyl(allegation_classification, action_classification)

# Long distribution of repercussions by misconduct type
# AY: didn't need the extra periods here
long_repercussion_by_allegation <- repercussion_by_allegation %>%
  pivot_longer(.,!allegation_classification, names_to = "action", values_to = "count") %>%
  filter(allegation_classification != "No Allegation Reported") %>%
  filter(action != "No Repercussion Reported") %>%
  filter(count != 0)

# Allegation types and percentage by police department
# AY: it seems to me like it would be more logical to group this by agency, rather than by allegation classification?

allegation_by_pd <- misconduct %>%
  tabyl(allegation_classification, agency_name) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2, affix_sign = FALSE) %>%
  mutate(across(everything(), ~ifelse(. == "0.00", "", .)))

# Disposition types and percentage by police department
disposition_by_pd <- misconduct %>%
  tabyl(disposition_classification, agency_name) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2, affix_sign = FALSE) %>%
  mutate(across(everything(), ~ifelse(. == "0.00", "", .)))

# Repercussion types and percentage by police department
repercussion_by_pd <- misconduct %>%
  tabyl(action_classification, agency_name) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2, affix_sign = FALSE) %>%
  mutate(across(everything(), ~ifelse(. == "0.00", "", .)))

# Defining the unique allegation types
# AY: you already pulled these when you made the allegation distribution

unique_allegations <- allegation_distribution %>%
  pull(allegation_classification)

# Definig lists for the loop below
officer_allegation_list = list()
agency_allegation_list = list()
agency_50_perc_list = list()

# Loop
for (allegation_value in unique_allegations){
  
  # Officers with the most of the given allegation
  people_per_allegation <- misconduct %>%
    filter(allegation_classification == allegation_value) %>%
    group_by(uid) %>%
    summarize(n = n(),
              name = unique(full_name)) %>%
    distinct(uid, .keep_all = TRUE) %>%
    arrange(desc(n)) %>%
    select(-c("uid"))
  
  officer_allegation_list[[allegation_value]] <- people_per_allegation
  
  # Agencies with the highest percent of the given allegation
  percent_allegation_by_agency <- misconduct %>% 
    group_by(agency_name) %>%
    summarize(percent_allegation = sum(allegation_classification == allegation_value)/n()*100) %>%
    arrange(desc(percent_allegation))
  
  agency_allegation_list[[allegation_value]] <- percent_allegation_by_agency
  
  # Counting the number of agencies for who over 50% of their allegations come from the given allegation category
  allegation_50_over <- nrow(filter(agency_allegation_list[[agency_value]], percent_allegation > 50))
  
  agency_50_perc_list[[allegation_value]] <- allegation_50_over
}

# Number of agencies using use of force
n_departments_uof <- length(unique(filter(misconduct, allegation_classification == "Excessive Use of Force")$agency_name))

# Number of officers using use of force
n_police_uof <- length(unique(filter(misconduct, allegation_classification == "Excessive Use of Force")$uid))

# Types of Misconduct by Race and Gender of the officers 
misconduct %>% 
  tabyl(allegation_classification, sex, race) %>%
  adorn_percentages("col")
