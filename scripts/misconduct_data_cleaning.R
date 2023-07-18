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

# Cleaning function to standardize allegations of misconduct
allegation_cleaner <- function(df) {
  df %>%
    mutate(
      
      # Removing unnecessary characters
      clean_allegation = tolower(allegation),
      clean_allegation = ifelse(
        str_detect(allegation, paste0("rule", "\\s?\\d+:")),
        str_extract(allegation, "(?<=: )[[:alnum:] ]+(?=[; -])"),
        str_extract(allegation, "(?<=\\d:)(.+?)(?=( - \\d+)?$)|(?<=^)(.+?)(?=( - \\d+)?$)")
      ),
      clean_allegation = gsub("^\\d+:\\d+\\s", "", clean_allegation),
      
      # Cleaning categories of allegations
      clean_allegation = str_replace_all(clean_allegation, ".*force.*", "use of force"),
      clean_allegation = str_replace_all(clean_allegation, ".*performance of duty.*", "performance of duty"),
      clean_allegation = str_replace_all(clean_allegation, ".*moral conduct.*", "moral conduct"),
      clean_allegation = str_replace_all(clean_allegation, ".*professionalism.*", "professionalism conduct"),
      clean_allegation = str_replace_all(clean_allegation, ".*reporting for duty.*", "reporting for duty"),
      clean_allegation = str_replace_all(clean_allegation, ".*conduct unbecoming.*", "conduct unbecoming of an officer"),
      clean_allegation = str_replace_all(clean_allegation, ".*damag.*", "damaging equipment"),
      clean_allegation = replace_na(clean_allegation, "unknown"),
      
      # Creating simplified allegation groups using the most common types of allegations
      allegation_simplified = ifelse(clean_allegation == "performance of duty" | 
                                       clean_allegation == "use of force" |
                                       clean_allegation == "conduct unbecoming of an officer"|
                                       clean_allegation == "moral conduct" | 
                                       clean_allegation == "professionalism conduct" | 
                                       clean_allegation == "unknown", clean_allegation, "other"),
      allegation_simplified = str_to_title(allegation_simplified)
    )
}

# Cleaning function standardize dispositions
disposition_cleaner <- function(df) {
  df %>%
    mutate(
      
      # Removing necessary characters
      clean_disposition = tolower(disposition),
      clean_disposition = gsub(";.*", "", clean_disposition),
      
      # Cleaning categories of dispositions
      clean_disposition = str_replace_all(clean_disposition, ".*termin.*", "terminated"),
      clean_disposition = str_replace_all(clean_disposition, ".*pend.*", "pending investigation"),
      clean_disposition = str_replace_all(clean_disposition, ".*not sustain.*", "not sustained"),
      clean_disposition = str_replace_all(clean_disposition, ".*non-sustain.*", "not sustained"),
      clean_disposition = str_replace_all(clean_disposition, ".*insufficient evidence.*", "not sustained"),
      clean_disposition = replace_na(clean_disposition, "no disposition recorded"),
      
      # Creating simplified disposition groups using Sustained, Not Sustained, Unknown, and Other
      disposition_simplified = ifelse(clean_disposition == "sustained", "sustained", 
                                      ifelse(clean_disposition == "no disposition recorded", "unknown", 
                                             ifelse(clean_disposition == "not sustained", "not sustained",
                                                    "other"))),
      disposition_simplified = str_to_title(disposition_simplified)
    )
}

# Cleaning function to standarize actions used against police in cases of misconduct
action_cleaner <- function(df) {
  df %>%
    mutate(
      
      # Cleaning categories of actions
      clean_action = tolower(action),
      clean_action = str_replace_all(clean_action, ".*suspen.*", "suspended"),
      clean_action = str_replace_all(clean_action, ".*letter.*", "written or verbal reprimand"),
      clean_action = str_replace_all(clean_action, ".*writ.*", "written or verbal reprimand"),
      clean_action = str_replace_all(clean_action, ".*reprimand.*", "written or verbal reprimand"),
      clean_action = str_replace_all(clean_action, ".*train.*", "training"),
      clean_action = str_replace_all(clean_action, ".*demot.*", "demoted"),
      clean_action = str_replace_all(clean_action, ".*loss of unit.*", "lost unit privileges"),
      clean_action = str_replace_all(clean_action, ".*counsel.*", "counseled"),
      clean_action = str_replace_all(clean_action, ".*termin.*", "terminated"),
      clean_action = str_replace_all(clean_action, ".*warn.*", "warned"),
      clean_action = str_replace_all(clean_action, ".*resig.*", "resigned"),
      clean_action = replace_na(clean_action, "unknown"),
      
      # Creating simplified action groups using the most common types of actions
      action_simplified = ifelse(clean_action == "written or verbal reprimand" | 
                                   clean_action == "suspended" |
                                   clean_action == "decertified"|
                                   clean_action == "terminated" | 
                                   clean_action == "resigned" | 
                                   clean_action == "unknown", clean_action, "other"),
      action_simplified = str_to_title(action_simplified)
    )
}

# Renaming "agency_slug" as "agency" in the location data so that we can merge below
names(agency_locations)[names(agency_locations) == 'agency_slug'] <- 'agency'

# Merging personnel, with allegations, and agency locations, getting full names of personnel, and applying our cleaning functions above
misconduct <- merge(personnel, allegations, all.y = TRUE, by = c("uid", "agency", "race", "sex")) %>% 
  unite(col='full_name', c('first_name', "middle_name", 'last_name') , sep = " ", na.rm = TRUE) %>%
  merge(., agency_locations, by = "agency") %>%
  allegation_cleaner(.) %>%
  disposition_cleaner(.) %>%
  action_cleaner(.)

# Number of misconducts in the data
n_misconduct <- nrow(misconduct)

# Number of police officers in the misconduct data
n_police <- length(unique(misconduct$uid))

# Number of police departments
n_departments <- length(unique(misconduct$agency_name))

# Representation of police departments in the data
department_count <- misconduct %>% 
  tabyl(agency_name) %>%
  arrange(desc(n))

# Map???

# % of agencies that reported race
percent_departments_race <- length(unique(filter(misconduct, !is.na(race))$agency_name))/n_departments

# % of agencies that reported gender
percent_departments_gender <- length(unique(filter(misconduct, !is.na(sex))$agency_name))/n_departments

# % of allegations that have reported repercussions
percent_allegation_repercussion <- nrow(filter(misconduct, !is.na(action)))/n_misconduct

# % of allegations that the officers name included
percent_allegation_officer <- nrow(filter(misconduct, full_name != ""))/n_misconduct

# % of allegations that have reported dispositions
percent_department_disposition <- length(unique(filter(misconduct, !is.na(disposition))$agency_name))/n_departments

# Incompleteness scores for each department
incompleteness_count <- misconduct %>%
  mutate(score = rowSums(is.na(.))) %>%
  group_by(agency_name) %>%
  summarize(score = mean(score)) %>%
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
percent_gender_male <- officer_sex_count[2,4]

# Percent of officers who are black
percent_race_black <- officer_race_count[2,4]

# Percent of officers who are white
percent_race_white <- officer_race_count[5,4]

# Average number of allegations per officer
average_allegations <- mean(tabyl(misconduct$uid)$n)

# Average number of allegations per sex
average_allegations_male <- mean(tabyl(filter(misconduct, sex == "male")$uid)$n)
average_allegations_female <- mean(tabyl(filter(misconduct, sex == "female")$uid)$n)
average_allegations_na <- mean(tabyl(filter(misconduct, is.na(sex))$uid)$n)

# Number of allegations in each category
allegation_distribution <- misconduct %>%
  tabyl(allegation_simplified)

# Number of disposition in each category
disposition_distribution <- misconduct %>%
  tabyl(disposition_simplified)

# Number of repercussions in each category
repercussion_distribution <- misconduct %>%
  tabyl(action_simplified)

# Number of use of force allegations
n_uof <- allegation_distribution[7,2]

# Percent of allegations that were sustained
percent_sustained <- disposition_distribution[3,3]

# Number of terminations
n_terminated <- repercussion_distribution[5,2]

# Percent of each disposition by allegation type
sustain_status_by_allegation_percet <- misconduct %>%
  tabyl(allegation_simplified, disposition_simplified) %>%
  adorn_percentages("row")

# Number of sustained per allegation type
sustained_by_allegation <- misconduct %>%
  tabyl(allegation_simplified, disposition_simplified) %>%
  select(c("allegation_simplified", "Sustained"))

# Distribution of repercussions by misconduct type
repercussion_by_allegation <- misconduct %>%
  tabyl(allegation_simplified, action_simplified) %>%
  select(-c("Unknown"))

# Percent of allegations that led to termination
percent_allegations_termination <- repercussion_distribution[5,3]

# Allegation types and count by police department
allegation_by_pd <- misconduct %>%
  tabyl(agency_name, allegation_simplified) 

# Disposition types and count by police department
disposition_by_pd <- misconduct %>%
  tabyl(disposition_simplified, allegation_simplified) 


# Repercussion types and count by police department
repercussion_by_pd <- misconduct %>%
  tabyl(action_simplified, allegation_simplified) 

# 250 officers with the most use of force allegations
officers_most_uof <- misconduct %>%
  filter(allegation_simplified == "Use Of Force") %>%
  group_by(uid) %>%
  summarize(n = n(),
            name = unique(full_name)) %>%
  distinct(uid, .keep_all = TRUE) %>%
  arrange(desc(n)) %>%
  head(250) %>%
  select(-c("uid"))

# Number of agencies using use of force
n_departments_uof <- length(unique(filter(misconduct, allegation_simplified == "Use Of Force")$agency_name))

# Number of officers using use of force
n_police_uof <- length(unique(filter(misconduct, allegation_simplified == "Use Of Force")$uid))

# Agencies with the highest percent of Use of Force allegations
percent_uof_by_agency <- misconduct %>% 
  group_by(agency_name) %>%
  summarize(percent_uof = sum(allegation_simplified == "Use Of Force")/n()*100) %>%
  arrange(desc(percent_uof))

# Agencies with over 20% of allegations being use of force
n_20_perc_uof <- nrow(filter(percent_uof_by_agency, percent_uof >= 20))