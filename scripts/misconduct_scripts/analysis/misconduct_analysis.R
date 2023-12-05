# ------------------------------------- Loading Packages and Data ------------------------------------------

# Loading Libraries
library(tidyverse)
library(janitor)

# ------------------------------------- Data Analysis Process ----------------------------------------------

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

# % of allegations that have reported repercussions
get_pct_var_reported(index, action)

# Incompleteness scores for each department. The scoring works by
# counting the number of incomplete values for the officers full name,
# birth year, race, gender, allegation, and disposition. We then sum this value
# for each row and average across for each department to give each department
# a score. 
incompleteness_score <- misconduct %>%
  mutate(empty_count = rowSums(across(all_of(c("full_name",
  "birth_year", "race", "sex", "allegation", "disposition")), is.na))) %>%
  group_by(agency_name) %>%
  summarize(score = mean(empty_count)) %>%
  mutate(score = rescale(-score)*100) %>%
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
  group_by(uid) %>%
  summarize(Name = unique(full_name),
            Agency = unique(agency_name),
            Total = n(),
            `Allegation(s)` = paste(allegation,collapse=', ')
            ) %>%
  distinct(uid, .keep_all = TRUE) %>%
  select(-c(uid))

write.csv(people_most_allegations, "allegations.csv")

# Percent of officers who are male
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

# Number of allegations in each category
allegation_distribution <- misconduct_allegation_long %>%
  filter(allegation_category != "No Allegation Reported") %>%
  tabyl(allegation_category)

# Number of disposition in each category
disposition_distribution <- misconduct %>%
  filter(disposition_category != "No Disposition Reported") %>%
  tabyl(disposition_category)

# Number of internal repercussions in each category
internal_repercussion_distribution <- misconduct_action_long %>%
  filter(!(action_category %in% c("Arrested", 
                                  "Decertified", 
                                  "No Repercussion Reported"))) %>%
  tabyl(action_category)

# Number of outside repercussions in each category
external_repercussion_distribution <- misconduct_action_long %>%
  filter(action_category %in% c("Arrested",
                                "Decertified",
                                "Convicted")) %>%
  tabyl(action_category)

# Allegations by department type
allegation_department_type <- misconduct_allegation_long %>%
  filter(allegation_category != "No Allegation Reported") %>%
  tabyl(allegation_category_simplified, agency_type)

# Dispositions by department type
disposition_department_type <- misconduct %>%
  filter(disposition_category != "No Disposition Reported") %>%
  tabyl(disposition_category_simplified, agency_type)

# Internal Repercussions by department type
internal_repercussion_department_type <- misconduct_action_long %>%
  filter(!(action_category %in% c("No Repercussion Reported"))) %>%
  tabyl(action_category_simplified, agency_type)

# External Repercussions by department type
external_repercussion_department_type <- misconduct_action_long %>%
  filter(action_category %in% c("Arrested","Decertified", "Convicted")) %>%
  tabyl(action_category, agency_type)


# Number of use of force allegations
n_uof <- allegation_distribution %>%
  filter(allegation_category == "Use of Force") %>%
  pull(n)

# Percent of allegations that were sustained
pct_sustained <- disposition_distribution %>%
  filter(disposition_category == "Sustained") %>%
  adorn_pct_formatting() %>%
  pull(percent)

# Number of terminations
n_terminated <- internal_repercussion_distribution %>%
  filter(action_category == "Terminated") %>%
  pull(n)


# Percent of allegations terminated
pct_terminated <- internal_repercussion_distribution %>%
  filter(action_category == "Terminated") %>%
  adorn_pct_formatting() %>%
  pull(percent)

# Number of arrests
n_arrests <- outside_repercussion_distribution %>%
  filter(action_category == "Arrested") %>%
  adorn_pct_formatting() %>%
  pull(n)

# Percent arrested
n_arrests <- outside_repercussion_distribution %>%
  filter(action_category == "Arrested") %>%
  adorn_pct_formatting() %>%
  pull(percent)

# Percent of each disposition by allegation type
sustain_status_by_allegation_percent <- misconduct_allegation_long %>%
  tabyl(disposition_category_simplified,allegation_category) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2, affix_sign = FALSE)

# Number of sustained per allegation type
sustained_by_allegation <- misconduct_allegation_long %>%
  tabyl(allegation_category, disposition_category_simplified)


# Distribution of internal repercussions by misconduct type
repercussion_by_allegation <- merge(misconduct_action_long, misconduct_allegation_long, by = "index") %>% 
  filter(!(action_category %in% c("Arrested", 
                                  "Decertified",
                                  "Convicted"))) %>%
  tabyl(allegation_category, action_category)

# Table of allegations, dispositions, and repercussions
result <- misconduct %>%
  mutate(
    full_name = str_to_title(full_name),
    allegation = str_to_title(allegation),
    disposition = str_to_title(disposition),
    action = str_to_title(action)
  ) %>%
  select(full_name, agency_name, allegation, disposition, action)

# External repercussions by allegation
external_repercussion_by_allegation <- merge(misconduct_action_long, misconduct_allegation_long, by = "index") %>% 
  filter((action_category %in% c("Arrested", 
                                  "Decertified",
                                  "Convicted"))) %>%
  tabyl(allegation_category, action_category)

# Long distribution of repercussions by misconduct type
long_repercussion_by_allegation <- repercussion_by_allegation %>%
  pivot_longer(.,!allegation_category, names_to = "action_category", values_to = "count") %>%
  filter(allegation_category != "No Allegation Reported") %>%
  filter(action_category != "No Repercussion Reported") %>%
  filter(count != 0) %>%
  group_by(allegation_category) %>%
  summarize(allegation = allegation_category,
            repercussion = action_category,
            count = count,
            percent = count/sum(count))

# Distribution of outside repercussions by misconduct type
outside_repercussion_by_allegation <- merge(misconduct_action_long, misconduct_allegation_long, by = "index") %>% 
  filter(action_category %in% c("Arrested", "Decertified", "Convicted")) %>%
  tabyl(allegation_category, action_category)

# Allegation types and count by police department
allegation_by_pd <- misconduct_allegation_long %>%
  tabyl(agency_name, allegation_category_simplified) %>%
  mutate(across(everything(), ~ifelse(. == "0", "", .))) %>%
  select(-c("No Allegation Reported"))

# Disposition types and count by police department
disposition_by_pd <- misconduct %>%
  tabyl(agency_name, disposition_category_simplified) %>%
  mutate(across(everything(), ~ifelse(. == "0", "", .)))

# Internal repercussion types and count by police department
internal_repercussion_by_pd <- misconduct_action_long %>%
  tabyl(agency_name, action_category_simplified) %>%
  mutate(across(everything(), ~ifelse(. == "0", "", .)))

# Outside repercussion types and count by police department
outside_repercussion_by_pd <- misconduct_action_long %>%
  mutate(action_category = ifelse(!(action_category %in% c("Arrested", "Decertified")),
                                  "No External Repercussion Reported",
                                  action_category),
         action_category = ifelse(action_category == "No Repercussion Reported", 
                                  "No External Repercussion Reported",
                                  action_category)) %>%
  tabyl(agency_name, action_category) %>%
  mutate(across(everything(), ~ifelse(. == "0", "", .)))

# Defining the unique allegation types
unique_allegations <- allegation_distribution %>%
  pull(allegation_category)

# Defining lists for the loop below
officer_allegation_list = list()
agency_allegation_list = list()

# Loop
for (allegation_value in unique_allegations){
  
  # Officers with the most of the given allegation
  people_per_allegation <- misconduct_allegation_long %>%
    filter(allegation_category == allegation_value) %>%
    distinct(uid, .keep_all = TRUE) %>%
    group_by(agency_name, full_name) %>%
    summarize(n = n()) %>%
    arrange(desc(n), .by_group = T) %>%
    head(250)
  
  officer_allegation_list[[allegation_value]] <- people_per_allegation
  
  # Agencies with the highest percent of the given allegation
  percent_allegation_by_agency <- misconduct_allegation_long %>% 
    group_by(agency_name) %>%
    summarize(percent_allegation = sum(allegation_category == allegation_value)/n()*100) %>%
    arrange(desc(percent_allegation))
  
  agency_allegation_list[[allegation_value]] <- percent_allegation_by_agency
}

# Defining the unique agencies types
unique_agencies <- sort(unique(misconduct$agency_name))

# Defining list for the loop below
officers_misconduct <- list()

# Loop
for (chosen_agency in unique_agencies) {
  
  # Creating a shortened name that can be exported in excel
  short_name <- str_sub(chosen_agency, 1, 31)
  
  # Finding officers with the most allegations per agency
  df <- misconduct %>%
    filter(agency_name == chosen_agency) %>%
    group_by(full_name) %>%
    count() %>%
    arrange(desc(n)) %>%
    rename(!!chosen_agency := n) 
  
  officers_misconduct[[short_name]] <- df
}