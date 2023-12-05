# ------------------------------------- Loading Packages and Data ------------------------------------------

# Loading in libraries
library(janitor)
library(lubridate)
library(tidycensus)
library(tidyverse)
library(zoo)

# Defining Links
killing_data_link <- "original_data/killing_original_data/Mapping Police Violence.csv"
agency_locations_link <- "original_data/misconduct_original_data/data_agency-reference-list.csv"

# Reading in data
killing_data <- read_csv(here::here(killing_data_link))
agency_locations <- read_csv(here::here(agency_locations_link))


# ------------------------------------- Cleaning Data Process ----------------------------------------------

# Filtering for Louisiana killings and cleaning
la_killing <- killing_data %>%
  
# Cleaning names
  clean_names() %>%
  
# Filtering for Louisiana killings
  filter(state == "LA") %>%
  
# Separating Parish from Parish names
  separate_wider_delim(county, delim = " Parish", names = c("parish", "extra"), too_few = "align_start") %>%
  
# Removing "extra" column
  select(-extra) %>%
  
# Fixing demographic variables
  mutate(race = ifelse(is.na(race), "Unknown Race", race),
         race = ifelse(race == "Unknown race", "Unknown Race", race),
         gender = ifelse(is.na(gender), "Unknown Gender", gender),
         age = ifelse(is.na(age), "Unknown Age", age),
         
# Fixing and defining year and month variables
         date = mdy(date),
         year = year(date),
         year_month = month(date),

# Creating an age category
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
  
  # Cleaning column names
  clean_names() %>%
  
  # Renaming variables
  rename(county = name,
         name = variable) %>%
  
  # Joining with race variables
  left_join(race_vars, by = "name") %>%
  
  # Selecting specific variables
  select(county, value, label) %>%
  pivot_wider(names_from = "label", values_from = "value", values_fn = sum) %>%
  
  # Cleaning names
  clean_names() %>%
  
  # Creating a variable "any_part_black" to calculate any demographics any part black
  mutate(any_part_black = rowSums(across(contains("black")))) %>%
  
  # Selecting specific variables
  select(parish = county, total, white_alone = total_population_of_one_race_white_alone, any_part_black) %>%
  
  # Separating Parish from Parish names
  separate_wider_delim(parish, delim = " Parish", names = c("parish", "extra"), too_few = "align_start") %>%
  
  # Removing "extra" column
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
  
  # Joining killing rates and census data
  left_join(census_data, by = "parish") %>%
  
  # Creating killing rate variables
  mutate(total_kill_rate = 100000 * (Total / total),
         black_kill_rate = 100000 * (Black / any_part_black),
         white_kill_rate = 100000 * (White / white_alone),
         
         # Creating a killing ratio variable
         ratio_bw = black_kill_rate / white_kill_rate) %>%
  
  # Selecting variables to keep
  select(parish, total_kill_rate, black_kill_rate, white_kill_rate, ratio_bw)

# Killings for every hundred thousand residents by demographic
killing_rate_total <- 100000 * nrow(la_killing)/sum(census_data$total)
killing_rate_black <- 100000 * sum(la_killing$race == "Black", na.rm = TRUE)/sum(census_data$any_part_black)
killing_rate_white <- 100000 * sum(la_killing$race == "White", na.rm = TRUE)/sum(census_data$white_alone)
killing_ratio_bw <- killing_rate_black/killing_rate_white

# Parishes with the most killings per hundred thousand total, black, and white residents
parish_most_total_kill_rate <- killings_rate_demographics %>%
  arrange(desc(total_kill_rate))

parish_most_black_kill_rate <- killings_rate_demographics %>%
  arrange(desc(black_kill_rate))

parish_most_white_kill_rate <- killings_rate_demographics %>%
  arrange(desc(white_kill_rate))

# Killings by parish and gender, race, and age
gender_by_parish <- la_killing %>%
  tabyl(parish, gender)

age_by_parish <- la_killing %>%
  tabyl(parish, age_category)

race_by_parish <- la_killing %>%
  tabyl(parish, race)

# Number of months the data collection has occurred
date1 <- as.Date("2013-01-01")
date2 <- as.Date("2023-07-21")
num_months <- interval(date1, date2) %/% months(1)

# Months without a police killing in Louisiana
months_no_killing <- num_months - length(unique(format(as.Date(la_killing$date), "%Y-%m")))

# Moving Timeline Killings per year by demographic
race_killing_per_year <- la_killing %>%
  tabyl(year, race) %>%
  
  # Aggregating values
  mutate(across(Asian:White, cumsum))

gender_killing_per_year <- la_killing %>%
  tabyl(year, gender) %>%
  
  # Aggregating values
  mutate(across(Female:Male, cumsum))

age_killing_per_year <- la_killing %>%
  tabyl(year, age_category) %>%
  
  # Aggregating values
  mutate(across(`<18`:`55+`, cumsum))

# Average age killed
ave_age_killed_per_year <- la_killing %>%
  mutate(age = as.numeric(age)) %>%
  group_by(year) %>%
  summarize(mean_age = mean(age, na.rm = T))

# Armed Status Barchart
arm_status_by_race <- la_killing %>%
  
  # Mutating "allegedly_armed" variable so that any allegation which includes "Allegedly" becomes "Allegedly Armed"
  mutate(allegedly_armed = ifelse(str_detect(allegedly_armed, "Allegedly"), "Allegedly Armed", allegedly_armed)) %>%
  tabyl(allegedly_armed, race) %>%
  adorn_totals(where = "col")

# Flee Status Barchart
fleeing_status <- la_killing %>%
  
  # Filtering data to post 2014 (this is when fleeing began being recorded)
  filter(year >= 2015) %>%
  
  # Creating a "fleeing" variable that defines not fleeing as if "wapo_flee" is empty or is "Not Fleeing"
  mutate(fleeing = if_else((is.na(wapo_flee) | wapo_flee == "Not Fleeing"), "Not Fleeing", "Fleeing")) %>%
  tabyl(race, fleeing) %>%
  adorn_totals(where = "row")

# Percent of people allegedly fleeing
pct_fleeing_status <- fleeing_status %>%
  adorn_percentages()

# Violent v Non-Violent
violent_crime_distribution <- la_killing %>%
  
  # Filtering data to post 2016 (this is when encounter types began being recorded)
  filter(year >= 2017) %>%
  
  # # Creating a binary "crime_status", classifications being:  "Violent Crime", "Non-Violent Crime"
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
  
  # Splitting agencies by comma and removing all unnecessary text from agency names
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
  
  # Fixing names to include Parish Sheriff's Office
  mutate(agency_name = ifelse(str_detect(agency_name, "Parish"),
                              agency_name,
                              gsub("Sheriff's Office", 
                                   "Parish Sheriff's Office", 
                                   agency_name))) %>%
  
  # Merging with "agency_locations" by "agency_name"
  merge(agency_locations, by = "agency_name", all = TRUE) %>%
  
  # Creating a column of whether the department is represented or not in the data
  mutate(agency_slug = ifelse(is.na(Total), 
                              "Not Represented in the Killing Data", 
                              "Represented in the Killing Data"))


# Mapping
mapping_department_killings <- la_killing %>%
  
  # Making a single "latlong" variable
  unite(latlong, c(latitude, longitude), sep = " ", remove = F) %>%
  select("date", "latlong", "agency_responsible") %>%
  arrange(date)

# Officers who killed people
officers_killing <- la_killing %>%
  
  # Creating an officer variable which removes unnecessary text and white spaces
  mutate(
    officers = gsub("\\s*\\([^\\)]*\\)\\s*", "", officer_names, perl = TRUE),
    officers = strsplit(officers, "\\s* and \\s*|,\\s*")
  ) %>%
  unnest(officers) %>%
  
  # Including the date the killing occured with the name
  mutate(officer_date = paste0(officers, ", " , date)) %>%
  
  # Removing officers who are not named
  drop_na(officers) %>%
  select(officer_date)

# Charge status distribution
charge_status <- la_killing %>%
  
  # Making an binary "officer_charged" variable with categories: "Criminal Charges", "No Known Charges"
  mutate(officer_charged = if_else(str_detect(tolower(officer_charged), "charged"), 
                                   "Criminal Charges", 
                                   "No Known Charges")) %>%
  tabyl(officer_charged)

# Disposition status distribution
disposition_status <- la_killing %>%
  mutate(
    
    # Making dispositions lowercase
    disposition_official = tolower(disposition_official),
    
    # Creating disposition categories
    disposition_fixed = case_when(
      str_detect(disposition_official, "charged") ~ "Charged",
      str_detect(disposition_official, "pending|under") ~ "Pending Investigation",
      str_detect(disposition_official, "justified") ~ "Justified",
      str_detect(disposition_official, "cleared") ~ "Cleared",
      str_detect(disposition_official, "family awarded") ~ "Family Awarded Money",
      str_detect(disposition_official, "unreported") ~ "Unreported",
      disposition_official %in% c("unknown", NA) ~ "Unknown",
      TRUE ~ disposition_official
    )
  ) %>%
  tabyl(disposition_fixed)
