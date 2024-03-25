# ------------------------------------- Loading Packages and Data ------------------------------------------

# Loading in libraries
library(janitor)
library(lubridate)
library(tidycensus)
library(tidyverse)
library(zoo)
library(clipr)

# Date of updated data
newest_date = "2024-03-25"

# Defining Links
killing_data_link <- paste0("data/killing_data/",newest_date,"/Mapping Police Violence.csv")
agency_locations_link <- "data/misconduct_data/data_agency-reference-list.csv"

# Reading in data
killing_data <- read_csv(here::here(killing_data_link))
agency_locations <- read_csv(here::here(agency_locations_link))


# ------------------------------------- Cleaning Data Process ----------------------------------------------

# API key
api_key = "8b0dc67a5d26f4d27b193904ac4ef087b0409b5e"

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
           age >= 18 & age < 35 ~ "18 - 34",
           age >= 35 & age < 55 ~ "35 - 54",
           age >= 55 ~ "55+",
           TRUE ~ NA),
         parish = ifelse(parish == "Dallas", "Rapides", parish),
         parish = str_replace(parish, "Saint", "St."),
         parish = ifelse(parish == "Acadiana", "Acadia", parish),
         parish = ifelse(city == "Monroe", "St. Tammany", parish)) %>%
  filter(!(name == "Omarr Jackson" & race == "White"))

# Loading in census data
census_api_key(api_key)
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
  tabyl(parish) %>%
  mutate(n = ifelse(n == 1, paste0(n, " Person Killed"), paste0(n, " People Killed")))

# Description of deaths in each parish
description_data <- la_killing %>%
  select(parish, name, age, description = circumstances) %>%
  unite(name_age, c(name, age), sep = ", ")

# Group by race
demographic_race <- la_killing %>%
  tabyl(race) %>%
  arrange(desc(n))

# Group by gender 
demographic_gender <- la_killing %>%
  tabyl(gender) %>%
  arrange(desc(n))

# Group by age category
demographic_age <- la_killing %>%
  tabyl(age_category) %>%
  arrange(desc(age_category))

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
killing_ratio_bw

# Parishes with the most killings per hundred thousand total, black, and white residents
parish_most_total_kill_rate <- killings_rate_demographics %>%
  mutate(
    ratio_bw = ifelse(is.na(ratio_bw) | is.infinite(ratio_bw), NA, ratio_bw),
    total_kill_rate = paste0(round(total_kill_rate, 2), " People Killed per 100,000 Residents"),
    black_kill_rate = paste0(round(black_kill_rate, 2), " Black People Killed per 100,000 Black Residents"),
    white_kill_rate = paste0(round(white_kill_rate, 2), " White People Killed per 100,000 White Residents"),
    ratio_bw = paste0(round(ratio_bw, 2), " Black People Killed per 100,000 Black Residents to 1 White Person Killed per 100,000 White Residents"),
    ratio_bw = ifelse(ratio_bw %in% c("0 Black People Killed per 100,000 Black Residents to 1 White Person Killed per 100,000 White Residents",
                    "NA Black People Killed per 100,000 Black Residents to 1 White Person Killed per 100,000 White Residents"),
                     "", ratio_bw
                     ),
    black_kill_rate = ifelse(black_kill_rate == "0 Black People Killed per 100,000 Black Residents", "", black_kill_rate),
    white_kill_rate = ifelse(white_kill_rate == "0 White People Killed per 100,000 White Residents", "", white_kill_rate)
  )

# Total rate map
total_parish_kill_rate <- parish_most_total_kill_rate %>%
  select(parish,total_kill_rate)

# Black rate map
black_parish_kill_rate <- parish_most_total_kill_rate %>%
  select(parish,black_kill_rate) %>%
  filter(black_kill_rate != "")

# White rate map
white_parish_kill_rate <- parish_most_total_kill_rate %>%
  select(parish,white_kill_rate) %>%
  filter(white_kill_rate != "")

# Ratio map
ratio_bw_parish_kill_rate <- parish_most_total_kill_rate %>%
  select(parish,ratio_bw) %>%
  filter(ratio_bw != "")

# Top 5
highest_kill_rate <- killings_rate_demographics %>%
  arrange(desc(total_kill_rate)) %>%
  select(parish,total_kill_rate) %>%
  mutate(total_kill_rate = round(total_kill_rate,2)) %>%
  head(5)

# Killings by parish and gender, race, and age
gender_by_parish <- la_killing %>%
  tabyl(parish, gender) %>%
  mutate_all(~ ifelse(. == 0, "", .))

age_by_parish <- la_killing %>%
  tabyl(parish, age_category) %>%
  mutate_all(~ ifelse(. == 0, "", .))

race_by_parish <- la_killing %>%
  tabyl(parish, race) %>%
  mutate_all(~ ifelse(. == 0, "", .))

# Number of months the data collection has occurred
date1 <- as.Date("2013-01-01")
date2 <- as.Date(newest_date) # Change for the newest update
num_months <- interval(date1, date2) %/% months(1)
num_months

# Months without a police killing in Louisiana
months_no_killing <- num_months - length(unique(format(as.Date(la_killing$date), "%Y-%m")))
length(unique(format(as.Date(la_killing$date), "%Y-%m")))


# Moving Timeline Killings per year by demographic
race_killing_per_year <- la_killing %>%
  tabyl(year, race) %>%
  
  # Aggregating values
  mutate(across(Asian:White, cumsum)) %>%
  t()

gender_killing_per_year <- la_killing %>%
  tabyl(year, gender) %>%
  
  # Aggregating values
  mutate(across(Female:Male, cumsum)) %>%
  t()

age_killing_per_year <- la_killing %>%
  tabyl(year, age_category) %>%
  
  # Aggregating values
  mutate(across(`<18`:`55+`, cumsum)) %>%
  t()

# Average age killed
ave_age_killed_per_year <- la_killing %>%
  mutate(age = as.numeric(age)) %>%
  group_by(year) %>%
  summarize(mean_age = mean(age, na.rm = TRUE))


# Armed Status Barchart
arm_status_by_race <- la_killing %>%
  
  # Mutating "allegedly_armed" variable so that any allegation which includes "Allegedly" becomes "Allegedly Armed"
  mutate(allegedly_armed = ifelse(str_detect(allegedly_armed, "Allegedly"), "Allegedly Armed", allegedly_armed)) %>%
  tabyl(allegedly_armed, race) %>%
  adorn_totals(where = "col") %>%
  select(allegedly_armed, "Total", "Black", "White", "Hispanic", "Asian", "Unknown Race")

# Flee Status Barchart
fleeing_status <- la_killing %>%
  
  # Filtering data to post 2014 (this is when fleeing began being recorded)
  filter(year >= 2015) %>%
  
  # Creating a "fleeing" variable that defines not fleeing as if "wapo_flee" is empty or is "Not Fleeing"
  mutate(fleeing = if_else((is.na(wapo_flee) | wapo_flee == "Not Fleeing"), "Not Fleeing", "Fleeing")) %>%
  tabyl(race, fleeing) %>%
  adorn_totals(where = "row") %>%
  arrange(desc('Not Fleeing')) %>%
  arrange(desc(Fleeing))

# Percent of people allegedly fleeing
pct_fleeing_status <- fleeing_status %>%
  adorn_percentages()

# Violent v Non-Violent
violent_crime_distribution <- la_killing %>%
  
  # Filtering data to post 2016 (this is when encounter types began being recorded)
  filter(year >= 2017) %>%
  
  # Creating a binary "crime_status", classifications being:  "Violent Crime", "Non-Violent Crime"
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
    agency_name = map(agency_name, ~ str_remove_all(.x, '^"|"$|[()]'))
  ) %>%
  unnest(agency_name) %>%
  mutate(
    agency_name = case_when(
      agency_name == "Caddo County Sheriff's Office" ~ "Caddo Parish Sheriff's Office",
      agency_name == "Calcasieu Parish Sheriff’s Office" ~ "Calcasieu Parish Sheriff's Office",
      agency_name == "East Baton Rouge Sheriff's Office" ~ "East Baton Rouge Parish Sheriff's Office",
      agency_name == "Hourma Police Department" ~ "Houma Police Department",
      agency_name == "Arcadia Parish Sheriff's Office" ~ "Acadia Parish Sheriff's Office",
      agency_name == "Jefferson Parish Police Department" ~ "Jefferson Parish Sheriff's Office",
      agency_name == "St. John the Baptist Parish Sheriff's Office" ~ "St. John Parish Sheriff's Office",
      agency_name == "Tangipahoa Parish Sheriff’s Office" ~ "Tangipahoa Parish Sheriff's Office",
      agency_name %in% c("Tangipahoa Parish Sheriff’s Office", "Tangipahoa Sheriff's Department") ~ "Tangipahoa Parish Sheriff's Office",
      agency_name == "Terrebonne Parish Sheriff's Department" ~ "Terrebonne Parish Sheriff's Office",
      agency_name == "U.S. Bureau of Investigation" ~ "U.S. Federal Bureau of Investigation",
      agency_name == "US Marshals" ~ "U.S. Marshals Service",
      TRUE ~ agency_name
    )
  ) %>%
  tabyl(agency_name, race) %>%
  adorn_totals(where = "col")

# Dep rate total
total_killing_per_dep <- killings_per_department %>%
  arrange(desc(Total)) %>%
  select(agency_name, Total) %>%
  head(5)

# Dep rate black
black_killing_per_dep <- killings_per_department %>%
  arrange(desc(Black)) %>%
  select(agency_name, Black) %>%
  head(5)

# Dep rate white
white_killing_per_dep <- killings_per_department %>%
  arrange(desc(White)) %>%
  select(agency_name, White) %>%
  head(5)

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

# Base Map
mapping_department_killings <- la_killing %>%
  
  # Making a single "latlong" variable
  unite(latlong, c(latitude, longitude), sep = " ", remove = F) %>%
  select("date", "latlong", "agency_responsible") %>%
  arrange(date)

# Map to copy
map <- data.frame(
  title = mapping_department_killings$agency_responsible,
  value = 1,
  group = "",
  coordinates = mapping_department_killings$latlong,
  label = mapping_department_killings$agency_responsible,
  start_date = mapping_department_killings$date,
  end_date = "",
  text = mapping_department_killings$date
)


# Mapping by type
type_map <- la_killing %>% 
  
  # Splitting agencies by comma and removing all unnecessary text from agency names
  mutate(
    agency_name = str_split(as.character(str_replace_all(agency_responsible, ", ", ",")), ","),
    agency_name = map(agency_name, ~ str_remove_all(.x, '^"|"$|[()]')),
    id = row_number()
  ) %>%
  unnest(agency_name) %>%
  mutate(agency_type = case_when(
    str_detect(tolower(agency_name), "university|college|campus") ~ "University or Campus Police",
    str_detect(tolower(agency_name), "marshal") ~ "Marshal's Office",
    str_detect(tolower(agency_name), "constable") ~ "Constable's Office",
    str_detect(tolower(agency_name), "sheriff") ~ "Sheriff's Office",
    str_detect(tolower(agency_name), "department|police department") ~ "Police Department",
    TRUE ~ "Other Law Enforcement Agency"
  )) %>%
  group_by(id) %>%
  summarize(agency_name = paste(agency_name, collapse = ", "),
            agency_type = paste(agency_type, collapse = ", "),
            latitude = unique(latitude),
            longitude = unique(longitude),
            date = unique(date)) %>%
  unite(latlong, c(latitude, longitude), sep = " ", remove = F) %>%
  mutate(agency_type = ifelse(str_detect(agency_type, ","), "Multiple Types", agency_type))


# Officers who killed people
officers_killing <- la_killing %>%
  
  # Creating an officer variable which removes unnecessary text and white spaces
  mutate(
    officers = gsub("\\s*\\([^\\)]*\\)\\s*", "", officer_names, perl = TRUE),
    officers = strsplit(officers, "\\s* and \\s*|,\\s*")
  ) %>%
  unnest(officers) %>%
  
  # Including the date the killing occured with the name
  #mutate(officer_date = paste0(officers, ", " , date)) %>%
  
  # Removing officers who are not named
  drop_na(officers) %>%
  select(officers)

sum(!is.na(la_killing$officer_names))

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
  tabyl(disposition_fixed) %>%
  arrange(desc(n))

# Pending distribution per year 
pending_over_time <- la_killing %>%
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
  filter(disposition_fixed == "Pending Investigation") %>%
  tabyl(year)
