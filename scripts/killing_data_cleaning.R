# ------------------------------------- Loading Packages and Data ------------------------------------------

# Loading in libraries
library(janitor)
library(lubridate)
library(tidycensus)
library(tidyverse)
library(zoo)
library(readxl)

# Reading in data 
killing_data <- read_csv(here::here("original_data/killing_original_data/Mapping Police Violence.csv"))
agency_locations <- read_csv(here::here("original_data/misconduct_original_data/data_agency-reference-list.csv"))


# ------------------------------------- Cleaning Data Process ----------------------------------------------

# Filtering for Louisiana killings and cleaning
la_killing <- killing_data %>%
  clean_names() %>%
  filter(state == "LA") %>%
  separate_wider_delim(county, delim = " Parish", names = c("parish", "extra"), too_few = "align_start") %>%
  select(-extra) %>%
  mutate(race = ifelse(is.na(race), "Unknown Race", race),
         race = ifelse(race == "Unknown race", "Unknown Race", race),
         gender = ifelse(is.na(gender), "Unknown Gender", gender),
         age = ifelse(is.na(age), "Unknown Age", age),
         date = mdy(date),
         year = year(date),
         year_month = month(date),
         age_category = case_when(
           age < 18 ~ "<18",
           age >= 18 & age <35 ~ "18 - 34",
           age >= 35 & age < 55 ~ "35 - 54",
           age >= 55 ~ "55+"),
         parish = ifelse(parish == "Dallas", "Rapides", parish),
         parish = str_replace(parish, "Saint", "St."),
         parish = ifelse(parish == "Acadiana", "Acadia", parish))

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
  select(-extra)


# ------------------------------------- Data Analysis Process ----------------------------------------------

# Number of killings per parish
killings_per_parish <- la_killing %>%
  tabyl(parish)

# Description of deaths in each parish
description_data <- la_killing %>%
  select(parish, name, age, description = circumstances) %>%
  unite(name_age, c(name, age), sep = ", ")

# Group by race
demographic_race <- la_killing %>%
  tabyl(race)

# Group by gender 
demographic_gender <- la_killing %>%
  tabyl(gender)

# Group by age category
demographic_age <- la_killing %>%
  tabyl(age_category)

# Percent of people killed who were black 
percent_killed_black <- demographic_race %>%
  filter(race == "Black") %>%
  adorn_pct_formatting() %>%
  pull(percent)

# Percent of people who are black in Louisiana population
percent_la_black <- sum(census_data$any_part_black)/sum(census_data$total)

# Percent of people killed who were male
percent_killed_male <- demographic_gender %>%
  filter(gender == "Male") %>%
  adorn_pct_formatting() %>%
  pull(percent)

# Average age of person killed in Louisiana
average_age_killed <- mean(as.integer(la_killing$age), na.rm = TRUE)

# Killing Rate & Demographics
killings_rate_demographics <- la_killing %>%
  tabyl(parish, race) %>%
  adorn_totals(where = "col") %>%
  left_join(census_data, by = "parish") %>%
  mutate(total_kill_rate = 100000 * (Total / total),
         black_kill_rate = 100000 * (Black / any_part_black),
         white_kill_rate = 100000 * (White / white_alone),
         ratio_bw = black_kill_rate / white_kill_rate) %>%
  select("parish", "total_kill_rate", "black_kill_rate", "white_kill_rate", "ratio_bw")

# Killings for every hundred thousand residents by demographic
killing_rate_total <- 100000 * nrow(la_killing)/sum(census_data$total)
killing_rate_black <- 100000 * sum(la_killing$race == "Black", na.rm = TRUE)/sum(census_data$any_part_black)
killing_rate_white <- 100000 * sum(la_killing$race == "White", na.rm = TRUE)/sum(census_data$white_alone)
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
  tabyl(parish, gender)

age_by_parish <- la_killing %>%
  tabyl(parish, age_category)

race_by_parish <- la_killing %>%
  tabyl(parish, race)

# Number of months the data collection has occured
date1 <- as.Date("2013-01-01")
date2 <- as.Date("2023-07-21")
num_months <- interval(date1, date2) %/% months(1)

# Months without a police killing in Louisiana
months_no_killing <- num_months - length(unique(format(as.Date(la_killing$date), "%Y-%m")))

# Moving Timeline Killings per year by demographic
race_killing_per_year <- la_killing %>%
  tabyl(year, race) %>%
  mutate(across(Asian:White, cumsum))

gender_killing_per_year <- la_killing %>%
  tabyl(year, gender) %>%
  mutate(across(Female:Male, cumsum))

age_killing_per_year <- la_killing %>%
  tabyl(year, age_category) %>%
  mutate(across(`<18`:`55+`, cumsum))

# Average killed per year by race
ave_black_killed_per_year <- mean(race_killing_per_year$Black)
ave_white_killed_per_year <- mean(race_killing_per_year$White)

# Average killed per year male
ave_men_killed_per_year <- mean(gender_killing_per_year$Male)

# Average age killed
ave_age_killed_per_year <- la_killing %>%
  mutate(age = as.numeric(age)) %>%
  group_by(year) %>%
  summarize(mean_age = mean(age, na.rm = T))

# Armed Status Barchart
arm_status_by_race <- la_killing %>%
  mutate(allegedly_armed = if_else(str_detect(allegedly_armed, "Allegedly"), "Allegedly Armed", allegedly_armed)) %>%
  tabyl(allegedly_armed, race) %>%
  adorn_totals(where = "col")

# Flee Status Barchart
fleeing_status <- la_killing %>%
  filter(year >= 2015) %>%
  mutate(fleeing = if_else((is.na(wapo_flee) | wapo_flee == "Not Fleeing"), "Not Fleeing", "Fleeing")) %>%
  tabyl(race, fleeing) %>%
  adorn_totals(where = "row")

# Percent of people allegedly fleeing
pct_fleeing_status <- fleeing_status %>%
  adorn_percentages()

pct_fleeing_status

# Violent v Non-Violent
violent_crime_distribution <- la_killing %>%
  filter(year >= 2017) %>%
  mutate(crime_status = if_else(encounter_type %in% c("Part 1 Violent Crime", 
                                                      "Part 1 Violent Crime/Domestic Disturbance"), 
                                "Violent Crime", 
                                "Non-Violent Crime")) %>%
  tabyl(crime_status)

# Counting mental health status groups 
mental_health <- la_killing %>%
  tabyl(signs_of_mental_illness)


# Police Department Graphs
killings_per_department <- la_killing %>% 
  mutate(
    agency_name = str_split(as.character(str_replace_all(agency_responsible, ", ", ",")), ","),
    agency_name = map(agency_name, ~ str_remove_all(.x, '^"|"$'))
  ) %>%
  unnest(agency_name) %>%
  tabyl(agency_name, race) %>%
  adorn_totals(where = "col")

# Police agencies represented
departments_represented <- killings_per_department %>%
  select("agency_name","Total") %>%
  mutate(agency_name = ifelse(str_detect(agency_name, "Parish"),
                              agency_name,
                              gsub("Sheriff's Office", 
                                   "Parish Sheriff's Office", 
                                   agency_name))) %>%
  merge(agency_locations, by = "agency_name", all = TRUE) %>%
  mutate(agency_slug = ifelse(is.na(Total), 
                              "Not Represented in the Killing Data", 
                              "Represented in the Killing Data"))


# Mapping
mapping_department_killings <- la_killing %>%
  unite(latlong, c(latitude, longitude), sep = " ", remove = F) %>%
  select("date", "latlong", "agency_responsible") %>%
  arrange(date)

# Officers who killed people
officers_killing <- la_killing %>%
  mutate(
    officers = gsub("\\s*\\([^\\)]*\\)\\s*", "", officer_names, perl = TRUE),
    officers = strsplit(officers, "\\s* and \\s*|,\\s*")
  ) %>%
  unnest(officers) %>%
  mutate(officer_date = paste0(officers, ", " , date)) %>%
  drop_na(officers) %>%
  select(officer_date)

# Charge status distribution
charge_status <- la_killing %>%
  mutate(officer_charged = if_else(str_detect(tolower(officer_charged), "charged"), "Criminal Charges", "No Known Charges")) %>%
  tabyl(officer_charged)

charge_status

# Disposition status distribution
disposition_status <- la_killing %>%
  tabyl(disposition_official)

disposition_status


