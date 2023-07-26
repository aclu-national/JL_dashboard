# Loading Libraries
library(tidyverse)
library(lubridate)
library(janitor)
library(zoo)
library(scales)

# Reading in data
allegations <- read_csv("original_data/misconduct_original_data/data_allegation.csv")
personnel <- read_csv("original_data/misconduct_original_data/data_personnel.csv")
agency_locations <- read_csv("original_data/misconduct_original_data/data_agency-reference-list.csv")
allegation_class <- read_csv("original_data/misconduct_original_data/classifiers/data_allegation_classification.csv")
disposition_class <- read_csv("original_data/misconduct_original_data/classifiers/data_disposition_classification.csv")
repercussion_class <- read_csv("original_data/misconduct_original_data/classifiers/data_repercussion_classification.csv")

# Renaming "agency_slug" as "agency" in the location data so that we can merge below
names(agency_locations)[names(agency_locations) == 'agency_slug'] <- 'agency'

# Getting full names of officers, merging with allegation, disposition, and repercussion categories, and replace empty values
misconduct <- merge(personnel, allegations, all.y = TRUE, by = c("uid", "agency", "race", "sex")) %>% 
  unite(col='full_name', c('first_name', "middle_name", 'last_name') , sep = " ", na.rm = TRUE) %>%
  merge(., agency_locations, by = "agency") %>%
  merge(., allegation_class, by = "allegation", all.x = TRUE) %>%
  merge(., disposition_class, by = "disposition", all.x=TRUE) %>%
  merge(., repercussion_class, by = "action", all.x=TRUE) %>%
  select(-c("allegation_categories", "allegation_categories_description", 
            "disposition_categories", "disposition_categories_description",
            "action_categories", "action_categories_description")) %>%
  mutate(allegation_classification = replace_na(allegation_classification, "No Allegation Reported"),
         disposition_classification = replace_na(disposition_classification, "No Disposition Reported"),
         action_classification = replace_na(action_classification, "No Repercussion Reported"),
         full_name = ifelse(full_name == "", "Unnamed Officer", full_name))

# Number of misconducts in the data
n_misconduct <- nrow(misconduct)

# Number of police officers in the misconduct data
n_police <- length(unique(misconduct$uid))

# Number of police departments
n_departments <- length(unique(misconduct$agency_name))

# Representation of police departments in the data
department_count <- misconduct %>% 
  tabyl(agency_name)
  
# % of agencies that reported race
percent_departments_race <- 100 * length(unique(filter(misconduct, !is.na(race))$agency_name))/n_departments

# % of agencies that reported gender
percent_departments_gender <- 100 * length(unique(filter(misconduct, !is.na(sex))$agency_name))/n_departments

# % of allegations that have reported repercussions
percent_allegation_repercussion <- 100 * nrow(filter(misconduct, !is.na(action)))/n_misconduct

# % of allegations that the officers name is included
percent_allegation_officer <- 100 * nrow(filter(misconduct, full_name != ""))/n_misconduct

# % of allegations that have reported dispositions
percent_department_disposition <- 100 * length(unique(filter(misconduct, !is.na(disposition))$agency_name))/n_departments

# Incompleteness scores for each department
incompleteness_score <- misconduct %>%
  mutate(score = rowSums(is.na(.))) %>%
  group_by(agency_name) %>%
  summarize(score = -mean(score)) %>%
  mutate(score = rescale(score)*100) %>%
  arrange(desc(score))

# Number of officers by race 
officer_race_count <- misconduct %>%
  group_by(uid) %>%
  summarize(count = n(),
            race = unique(race)) %>%
  distinct(uid, .keep_all = TRUE) %>%
  tabyl(race)

# Number of officers by gender 
officer_sex_count <- misconduct %>%
  group_by(uid) %>%
  summarize(n = n(),
            sex = unique(sex)) %>%
  distinct(uid, .keep_all = TRUE) %>%
  tabyl(sex)

# People with most allegations of misconduct per department
people_list = list()
for (agency_choice in sort(unique(misconduct$agency_name))){
  people_per_department <- misconduct %>%
    filter(agency_name == agency_choice) %>%
    group_by(uid) %>%
    summarize(n = n(),
              name = unique(full_name)) %>%
    distinct(uid, .keep_all = TRUE) %>%
    tabyl(name) %>%
    arrange(desc(n))
    
  people_list[[agency_choice]] <- people_per_department
}

# Percent of officers who are male
percent_gender_male <- 100 * officer_sex_count[2,4]

# Percent of officers who are black
percent_race_black <- 100 * officer_race_count[2,4]

# Percent of officers who are white
percent_race_white <- 100 * officer_race_count[5,4]

# Average number of allegations per officer
average_allegations <- mean(tabyl(misconduct$uid)$n)

# Average number of allegations per sex
average_allegations_male <- mean(tabyl(filter(misconduct, sex == "male")$uid)$n)
average_allegations_female <- mean(tabyl(filter(misconduct, sex == "female")$uid)$n)
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
n_uof <- allegation_distribution[2,2]

# Percent of allegations that were sustained
percent_sustained <- 100 * disposition_distribution[7,3]

# Number of terminations
n_terminated <- repercussion_distribution[13,2]

# Percent of allegations terminated
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
long_repercussion_by_allegation <- repercussion_by_allegation %>%
  pivot_longer(.,!allegation_classification, names_to = "action", values_to = "count") %>%
  filter(., allegation_classification != "No Allegation Reported") %>%
  filter(., action != "No Repercussion Reported") %>%
  filter(., count != 0)

# Allegation types and percentage by police department
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
unique_allegations <- sort(unique(misconduct$allegation_classification))

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
