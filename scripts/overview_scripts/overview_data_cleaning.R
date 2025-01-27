# ------------------------------------- Loading Packages and Data ------------------------------------------

# Loading Libraries
library(lubridate)
library(janitor)
library(zoo)
library(tidyverse)

newest_date <- "2025-01-27"
# Defining Links
pd_sizes_link = paste0("data/overview_data/", newest_date,"/lee_1960_2023.csv")
agency_locations_link = "data/misconduct_data/data_agency-reference-list.csv"
pd_references_link = "data/overview_data/35158-0001-Data.rda"

# Reading in data
pd_sizes <- read_csv(here::here(pd_sizes_link))
agency_locations <- read_csv(here::here(agency_locations_link))

# Loading in data
load(here::here(pd_references_link))

pd_references <- da35158.0001

# ------------------------------------- Cleaning Data Process ----------------------------------------------

# Renaming variables in the pd references
pd_references <- pd_references %>%
  select(ORI9, NAME) %>%
  rename(ori = ORI9,
         agency_full_name = NAME)


# Defining the type of agency
agency_locations <- agency_locations %>%
  filter(!(agency_slug %in% c("de-soto-so", "new-orleans-so"))) %>%
  mutate(
    agency_type = case_when(
    str_detect(tolower(agency_name), "university|college|campus") ~ "University or Campus Police",
    str_detect(tolower(agency_name), "marshal") ~ "Marshal's Office",
    str_detect(tolower(agency_name), "constable") ~ "Constable's Office",
    str_detect(tolower(agency_name), "sheriff") ~ "Sheriff's Office",
    str_detect(tolower(agency_name), "department|police department") ~ "Police Department",
    TRUE ~ "Other Law Enforcement Agency"
  ))

# Connecting department references with police department sizes
la_pd_sizes <- pd_sizes %>% 
  filter(state_abbr == "LA") %>%
  left_join(pd_references, by = "ori") %>%
  mutate(agency_name = str_trim(str_to_title(agency_full_name)),
         agency_name = ifelse(is.na(agency_name), pub_agency_name, agency_name),
         agency_name = str_replace(agency_name, "Dept|Dept.|Pd", "Police Department"),
         agency_name = str_remove(agency_name, "\\.$"))

# Filtering data for just 2023
la_pd_sizes_2023 <- la_pd_sizes %>%
  filter(data_year == "2023")

# ------------------------------------- Data Analysis Process ----------------------------------------------

# Mapping police departments
agency_map <- agency_locations %>%
  select(agency_name, agency_type, location)


# Distribution of agency types
agency_distribution <- agency_locations %>%
  count(agency_type)

# Number of sheriff offices
n_so <- agency_distribution %>%
  filter(agency_type == "Sheriff's Office") %>%
  pull(n)

# Number of officers over time
officers_over_time <- la_pd_sizes %>%
  pivot_wider(names_from = data_year, values_from = officer_ct) %>%
  select(-c("ori", "pub_agency_unit", "state_abbr",
            "division_name", "region_name", "county_name", 
            "agency_type_name", "population_group_desc", 
            "population", "male_officer_ct", "male_civilian_ct",
            "male_total_ct", "female_officer_ct", "female_civilian_ct",
            "female_total_ct", "civilian_ct",
            "total_pe_ct", "pe_ct_per_1000", "agency_full_name",
            "pub_agency_name")) %>%
  group_by(agency_name) %>%
  fill(2:63, .direction = 'updown') %>%
  distinct(agency_name, .keep_all = TRUE) %>%
  select(63:2) %>%
  arrange(agency_name)

# Mapping average number of officers per agency
average_agency_map <- la_pd_sizes_2023 %>% 
  separate_rows(county_name, sep = ", ") %>%
  group_by(county_name) %>%
  summarize(pct_per_county = mean(total_pe_ct)) %>%
  arrange(-pct_per_county) %>%
  mutate(county_name = county_name %>% str_to_title(),
         county_name = str_replace(county_name, "St ", "St. "),
         pct_per_county = paste0(round(pct_per_county,2), " Officers per Reporting Department")
         )

# Increase in officers per law enforcement agency
average_increase <- la_pd_sizes %>%
  group_by(data_year) %>%
  filter(data_year %in% c("1960", "2023")) %>%
  summarize(ave_officers = mean(total_pe_ct))

# Plotting the average number of offers per 100,000 residents 
officers_per_residents <- la_pd_sizes %>%
  group_by(data_year) %>%
  summarize(ave_per_hundredthousand = 100 * median(pe_ct_per_1000, na.rm = TRUE))

# Number of agencies in 2022
n_agencies_2023 = length(unique(la_pd_sizes_2023$agency_name))

# Number of officers in 2022
n_officers_2023 = sum(la_pd_sizes_2023$total_pe_ct)

# Number of agencies throughout time
n_agencies = length(unique(la_pd_sizes$agency_name))

# Number of police departments
n_pd <- agency_distribution %>%
  filter(agency_type == "Police Department") %>%
  pull(n)
