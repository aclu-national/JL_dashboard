# Loading in libraries
library(janitor)
library(lubridate)
library(tidycensus)
library(tidyverse)
library(zoo)
library(readxl)

# Reading in data 
killing_data <- read_excel("original_data/killing_original_data/JL_killed_raw_06092023.xlsx")

# Filtering for Louisiana killings and cleaning
la_killing <- killing_data %>%
  clean_names() %>%
  filter(state == "LA") %>%
  separate_wider_delim(county, delim = " Parish", names = c("parish", "extra"), too_few = "align_start") %>%
  select(-extra) %>%
  mutate(victims_race = if_else(victims_race == "Unknown race", "Unknown Race", victims_race),
         year = year(date_of_incident_month_day_year),
         year_month = month(date_of_incident_month_day_year),
         age_category = case_when(
           victims_age < 18 ~ "<18",
           victims_age >= 18 & victims_age <35 ~ "18 - 34",
           victims_age >= 35 & victims_age < 55 ~ "35 - 54",
           victims_age >= 55 ~ "55+"))

# Loading in census data
census_api_key("8b0dc67a5d26f4d27b193904ac4ef087b0409b5e")
vars_2020 <- load_variables(2020, "pl")

# Filter census data for race
race_vars <- vars_2020 %>%
  filter(concept == "RACE")

# Name the race columns
v = race_vars$name

# Creating a census data that counts the number of black and white people per parish
census_data <- get_decennial(geography = "county", variables = v, year = "2020", sumfile = "pl", state = "Louisiana") %>%
  clean_names() %>%
  rename(county = name,
         name = variable) %>%
  left_join(race_vars, by = "name") %>%
  select(county, value, label) %>%
  pivot_wider(names_from = "label", values_from = "value", values_fn = sum) %>%
  clean_names() %>%
  mutate(any_part_black = rowSums(across(contains("black")))) %>%
  select(parish = county, total, white_alone = total_population_of_one_race_white_alone, any_part_black) %>%
  separate_wider_delim(parish, delim = " Parish", names = c("parish", "extra"), too_few = "align_start") %>%
  mutate(parish = if_else(parish == "St. John the Baptist", "St. John The Baptist", parish)) %>%
  select(-extra)

# Number of killings per parish
killings_per_parish <- la_killing %>%
  tabyl(parish)

# Description of deaths in each parish
description_data <- la_killing %>%
  select(parish, name = victims_name, age = victims_age, description = a_brief_description_of_the_circumstances_surrounding_the_death) %>%
  unite(name_age, c(name, age), sep = ", ")

# Demographic Breakdown

# Group by race
demographic_race <- la_killing %>%
  tabyl(victims_race)

# Group by gender 
demographic_gender <- la_killing %>%
  tabyl(victims_gender)

# Group by age category
demographic_age <- la_killing %>%
  tabyl(age_category)

# Percent of people killed who were black 
percent_killed_black <- demographic_race[2,3]

# Percent of people who are black in Louisiana population
percent_la_black <- sum(census_data$any_part_black)/sum(census_data$total)

# Percent of people killed who were male
percent_killed_male <- demographic_gender[2,3]

# Average age of person killed in Louisiana
average_age_killed <- mean(as.integer(la_killing[!(la_killing$victims_age == "Unknown"),]$victims_age))

# Killing Rate & Demographics
killings_rate_demographics <- la_killing %>%
  tabyl(parish, victims_race) %>%
  adorn_totals(where = "col") %>%
  left_join(census_data, by = "parish") %>%
  mutate(total_kill_rate = 100000 * (Total / total),
         black_kill_rate = 100000 * (Black / any_part_black),
         white_kill_rate = 100000 * (White / white_alone),
         ratio_bw = black_kill_rate / white_kill_rate) %>%
  select("parish", "total_kill_rate", "black_kill_rate", "white_kill_rate", "ratio_bw")

# Killings for every hundred thousand residents by demographic
killing_rate_total <- 100000 * nrow(la_killing)/sum(census_data$total)
killing_rate_black <- 100000 * sum(la_killing$victims_race == "Black")/sum(census_data$any_part_black)
killing_rate_white <- 100000 * sum(la_killing$victims_race == "White")/sum(census_data$white_alone)
killing_ratio_bw <- killing_rate_black/killing_rate_white

# Parishes with the most killings per hundred thousand total, black, and white residents
parish_most_total_kill_rate <- killings_rate_demographics %>%
  arrange(desc(total_kill_rate)) %>%
  head(5)

parish_most_black_kill_rate <- killings_rate_demographics %>%
  arrange(desc(black_kill_rate)) %>%
  head(5)

parish_most_white_kill_rate <- killings_rate_demographics %>%
  arrange(desc(white_kill_rate)) %>%
  head(5)

# Killings by parish and gender, race, and age
gender_by_parish <- la_killing %>%
  tabyl(parish, victims_gender)

age_by_parish <- la_killing %>%
  tabyl(parish, age_category)

race_by_parish <- la_killing %>%
  tabyl(parish, victims_race)





# Months without a police killing in Louisiana
months_no_killing <- 125 - length(unique(format(as.Date(la_killing$date_of_incident_month_day_year), "%Y-%m")))


# Moving Timeline Killings per year by demographic
race_killing_per_year <- la_killing %>%
  tabyl(year, victims_race)

gender_killing_per_year <- la_killing %>%
  tabyl(year, victims_gender)

age_killing_per_year <- la_killing %>%
  tabyl(year, age_category)

# Average killed per year by race
ave_black_killed_per_year <- mean(race_killing_per_year$Black)
ave_white_killed_per_year <- mean(race_killing_per_year$White)

# Average killed per year male
ave_men_killed_per_year <- mean(gender_killing_per_year$Male)

# Average age killed
ave_age_killed_per_year <- la_killing %>%
  mutate(victims_age = as.numeric(victims_age)) %>%
  group_by(year) %>%
  summarize(mean_age = mean(victims_age, na.rm = T))

# Armed Status Barchart
arm_status_by_race <- la_killing %>%
  mutate(armed_unarmed_status = if_else(str_detect(armed_unarmed_status, "Allegedly"), "Allegedly Armed", armed_unarmed_status)) %>%
  tabyl(armed_unarmed_status, victims_race) %>%
  adorn_totals(where = "col")

# Flee Status Barchart
fleeing_status <- la_killing %>%
  filter(year >= 2015) %>%
  mutate(fleeing = if_else((is.na(fleeing_source_wa_po_and_review_of_cases_not_included_in_wa_po_database) | fleeing_source_wa_po_and_review_of_cases_not_included_in_wa_po_database == "Not Fleeing"), "Not Fleeing", "Fleeing")) %>%
  tabyl(victims_race, fleeing) %>%
  adorn_totals(where = "row")

# Percent of people allegedly fleeing
fleeing_status[6,2]/nrow(filter(la_killing, year >= 2015))

# Violent v Non-Violent
violent_crime_distribution <- la_killing %>%
  filter(year >= 2017) %>%
  mutate(crime_status = if_else(encounter_type_draft == "Part 1 Violent Crime" | encounter_type_draft == "Part 1 Violent Crime/Domestic Disturbance", "Violent Crime", "Non-Violent Crime")) %>%
  tabyl(crime_status)

# Mental Health Pie Chart
# Counting mental health status groups 
mental_health <- la_killing %>%
  tabyl(symptoms_of_mental_illness)

# Police Department Graphs
killings_per_department <- la_killing %>% 
  mutate(departments = strsplit(as.character(agency_responsible_for_death), ", ")) %>%
  unnest(departments) %>%
  tabyl(departments, victims_race) %>%
  adorn_totals(where = "col")

# Mapping
mapping_department_killings <- la_killing %>%
  unite(latlong, c(latitude, longitude), sep = " ", remove = F) %>%
  select("date_of_incident_month_day_year", "latlong", "agency_responsible_for_death")

# Charge status distribution
charge_status <- la_killing %>%
  mutate(criminal_charges = if_else(str_detect(criminal_charges, "Charged"), "Criminal Charges", "No Known Charges")) %>%
  tabyl(criminal_charges)

# Disposition status distribution
la_killing %>%
  tabyl(official_disposition_of_death_justified_or_other)