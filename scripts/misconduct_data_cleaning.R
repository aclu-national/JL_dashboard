# Loading Libraries
library(tidyverse)
library(lubridate)
library(janitor)
library(zoo)
library(scales)

# Reading in data
allegations <- read_csv(here::here("original_data/misconduct_original_data/LLEAD_data/data_allegation.csv"))
personnel <- read_csv(here::here("original_data/misconduct_original_data/LLEAD_data/data_personnel.csv"))
agency_locations <- read_csv(here::here("original_data/misconduct_original_data/LLEAD_data/data_agency-reference-list.csv"))
allegation_class <- read_csv(here::here("original_data/misconduct_original_data/classifiers/data_allegation_classification.csv"))
disposition_class <- read_csv(here::here("original_data/misconduct_original_data/classifiers/data_disposition_classification.csv"))
repercussion_class <- read_csv(here::here("original_data/misconduct_original_data/classifiers/data_repercussion_classification.csv"))
pd_sizes <- read_csv(here::here("original_data/misconduct_original_data/department_size/pe_1960_2022.csv"))
load(here::here("original_data/misconduct_original_data/department_size/ICPSR_35158/DS0001/35158-0001-Data.rda"))


# Renaming "agency_slug" as "agency" in the location data so that we can merge below
agency_locations <- agency_locations %>%
  rename(agency = agency_slug)

# Getting full names of officers, merging with allegation, disposition, and repercussion categories, and replace empty values
misconduct <- personnel %>%
  unite(col = 'full_name', c('first_name', "middle_name", 'last_name'), sep = " ", na.rm = TRUE, remove = FALSE) %>%
  right_join(allegations, by = c("uid", "agency", "race", "sex")) %>%
  left_join(agency_locations, by = "agency") %>%
  left_join(allegation_class, by = "allegation") %>%
  left_join(disposition_class, by = "disposition") %>%
  left_join(repercussion_class, by = "action") %>%
  select(-c("allegation_categories", "allegation_categories_description", 
            "disposition_categories", "disposition_categories_description", 
            "action_categories", "action_categories_description")) %>%
         mutate(outside_action_classification = ifelse(action_classification %in% c("Arrest", "Resignation", "Decertification"),
                                                action_classification, NA),
         action_classification = ifelse(action_classification %in% c("Arrest", "Resignation", "Decertification"),
                                        NA, action_classification),
         index = row_number())

# Cleaning police department references
pd_references <- da35158.0001 %>%
  select("ORI9", "NAME") %>%
  rename("ori" = ORI9,
         "agency_full_name" = NAME)

# Connecting department references with police department sizes
la_pd_sizes <- pd_sizes %>% 
  filter(state_abbr == "LA") %>%
  left_join(pd_references, by = "ori") %>%
  mutate(agency_name = str_trim(str_to_title(agency_full_name)),
         agency_name = ifelse(is.na(agency_name), pub_agency_name, agency_name))
  
  
# Number of allegations in the data
n_misconduct <- nrow(misconduct)

# Number of police officers in the misconduct data
n_police <- length(unique(misconduct$uid))

# Number of police departments
n_departments <- length(unique(misconduct$agency_name))

# Representation of police departments in the data
department_count <- misconduct %>% 
  tabyl(agency_name) %>%
  arrange(desc(n))

# Defining a function to get the percent reported 
get_pct_var_reported <- function(group, variable) {
  misconduct %>%
    tabyl({{ group }}, {{ variable }}) %>%
    clean_names() %>%
    mutate(non_na_sum = rowSums(across(!c({{ group }}, na))),
           var_reported = if_else(non_na_sum > 0, TRUE, FALSE)) %>%
    tabyl(var_reported)
}

# % of agencies that reported race
get_pct_var_reported(agency_name, race)

# % of agencies that reported gender
get_pct_var_reported(agency_name, sex)

# % of officers whose names are included
get_pct_var_reported(index, full_name)

# % of allegations that have reported dispositions
get_pct_var_reported(index, disposition)

# Incompleteness scores for each department
incompleteness_score <- misconduct %>%
  mutate(score = rowSums(is.na(.))) %>%
  group_by(agency_name) %>%
  summarize(score = -mean(score)) %>%
  mutate(score = rescale(score)*100) %>%
  arrange(desc(score))

# Number of officers by race 
officer_race_count <- misconduct %>%
  distinct(uid, .keep_all = T) %>%
  tabyl(race)

# Number of officers by gender 
officer_sex_count <- misconduct %>%
  distinct(uid, .keep_all = T) %>%
  tabyl(sex)

# People with most allegations of misconduct per department
people_most_allegations <- misconduct %>%
  distinct(uid, .keep_all = TRUE) %>%
  group_by(agency_name, full_name) %>%
  summarize(n = n()) %>%
  arrange(desc(n), .by_group = T)

# Percent of officers who are male
pct_officers_male <- officer_sex_count %>%
  filter(sex == "male") %>%
  adorn_pct_formatting() %>%
  pull(valid_percent)

# Percent of officers who are black
pct_officers_black <- officer_race_count %>%
  filter(race == "black") %>%
  adorn_pct_formatting() %>%
  pull(valid_percent)

# Percent of officers who are white
pct_officers_white <- officer_race_count %>%
  filter(race == "white") %>%
  adorn_pct_formatting() %>%
  pull(valid_percent)

# Average number of allegations per officer
average_allegations <- mean(tabyl(misconduct$uid)$n)

# Number of allegations in each category
allegation_distribution <- misconduct %>%
  tabyl(allegation_classification)

# Number of disposition in each category
disposition_distribution <- misconduct %>%
  tabyl(disposition_classification)

# Number of internal repercussions in each category
repercussion_distribution <- misconduct %>%
  tabyl(action_classification)

# Number of outside repercussions in each category
outside_repercussion_distribution <- misconduct %>%
  tabyl(outside_action_classification)

# Number of use of force allegations
n_uof <- allegation_distribution %>%
  filter(allegation_classification == "Use of Force") %>%
  pull(n)

# Percent of allegations that were sustained
pct_sustained <- disposition_distribution %>%
  filter(disposition_classification == "Sustained") %>%
  adorn_pct_formatting() %>%
  pull(valid_percent)

# Number of terminations
n_terminated <- repercussion_distribution %>%
  filter(action_classification == "Termination") %>%
  pull(n)

# Percent of allegations terminated
pct_terminated <- repercussion_distribution %>%
  filter(action_classification == "Termination") %>%
  adorn_pct_formatting() %>%
  pull(valid_percent)

# Number of arrests
n_arrests <- outside_repercussion_distribution %>%
  filter(outside_action_classification == "Arrest") %>%
  adorn_pct_formatting() %>%
  pull(n)

# Percent arrested
n_arrests <- outside_repercussion_distribution %>%
  filter(outside_action_classification == "Arrest") %>%
  adorn_pct_formatting() %>%
  pull(valid_percent)

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
  drop_na(action_classification) %>%
  tabyl(allegation_classification, action_classification)

# Long distribution of repercussions by misconduct type
long_repercussion_by_allegation <- repercussion_by_allegation %>%
  pivot_longer(.,!allegation_classification, names_to = "action", values_to = "count") %>%
  filter(allegation_classification != "No Allegation Reported") %>%
  filter(action != "No Repercussion Reported") %>%
  filter(count != 0)

# Allegation types and count by police department
allegation_by_pd <- misconduct %>%
  tabyl(agency_name, allegation_classification) %>%
  mutate(across(everything(), ~ifelse(. == "0", "", .))) %>%
  rename("No Allegation Reported" = NA_)

# Disposition types and count by police department
disposition_by_pd <- misconduct %>%
  tabyl(agency_name, disposition_classification) %>%
  mutate(across(everything(), ~ifelse(. == "0", "", .))) %>%
  rename("No Disposition Reported" = NA_)

# Internal repercussion types and count by police department
repercussion_by_pd <- misconduct %>%
  tabyl(agency_name, action_classification) %>%
  mutate(across(everything(), ~ifelse(. == "0", "", .))) %>%
  rename("No Internal Repercussion Reported" = NA_)

# Outside repercussion types and count by police department
outside_repercussion_by_pd <- misconduct %>%
  tabyl(agency_name, outside_action_classification) %>%
  mutate(across(everything(), ~ifelse(. == "0", "", .))) %>%
  rename("No Outside Repercussion Reported" = NA_)

# Defining the unique allegation types
unique_allegations <- allegation_distribution %>%
  pull(allegation_classification)

# Defining lists for the loop below
officer_allegation_list = list()
agency_allegation_list = list()

# Loop
for (allegation_value in unique_allegations){
  
  # Officers with the most of the given allegation
  people_per_allegation <- misconduct %>%
    filter(allegation_classification == allegation_value) %>%
    distinct(uid, .keep_all = TRUE) %>%
    group_by(agency_name, full_name) %>%
    summarize(n = n()) %>%
    arrange(desc(n), .by_group = T) %>%
    head(250)
  
  officer_allegation_list[[allegation_value]] <- people_per_allegation
  
  # Agencies with the highest percent of the given allegation
  percent_allegation_by_agency <- misconduct %>% 
    group_by(agency_name) %>%
    summarize(percent_allegation = sum(allegation_classification == allegation_value)/n()*100) %>%
    arrange(desc(percent_allegation))
  
  agency_allegation_list[[allegation_value]] <- percent_allegation_by_agency
}

# Allegations sustained versus not sustained
sustained_vs_not <- misconduct %>%
  tabyl(allegation_classification, disposition_classification) %>%
  adorn_totals("col") %>%
  select("allegation_classification","Sustained", "Total")

# Outside repercussions by allegation type
outside_action_by_allegation <- misconduct %>%
  tabyl(allegation_classification, outside_action_classification)

# Count and average of officers demographics per year
officers_per_year <- la_pd_sizes %>%
  group_by(data_year) %>%
  summarize(n_officers = sum(total_pe_ct),
            ave_officers = mean(total_pe_ct),
            n_male_officers = sum(male_total_ct),
            ave_male_officers = mean(male_total_ct),
            n_female_officers = sum(female_total_ct),
            ave_female_officers = mean(female_total_ct),
            n_departments = n())

# Mapping police officers
la_pd_sizes %>% 
  filter(data_year == "2022") %>%
  separate_rows(county_name, sep = ", ") %>%
  group_by(county_name) %>%
  summarize(n_per_county = sum(total_pe_ct),
            pct_per_county = mean(total_pe_ct),
            pct_1000_per_county = mean(pe_ct_per_1000, na.rm = TRUE)) %>%
  mutate(county_name = county_name %>% str_to_title())

# Officer demographics in each agency
agency_sizes = list()
for (agency in sort(unique(la_pd_sizes$agency_name))){
  df <- la_pd_sizes %>% 
    filter(agency_name == agency) %>%
    select("data_year","male_total_ct", "female_total_ct", "total_pe_ct") %>%
    rename("data_year" = data_year,
           "Total Male Officers" = male_total_ct, 
           "Total Female Officers" = female_total_ct,
           "Total Officers" = total_pe_ct) %>%
    arrange(desc(-data_year))
  
  agency_sizes[[agency]] <- df
}

# Average number of officers per 1000 people per year
officers_1000_per_year <- la_pd_sizes %>%
  group_by(data_year) %>%
  summarize(ave_pct_100_officers = mean(pe_ct_per_1000))