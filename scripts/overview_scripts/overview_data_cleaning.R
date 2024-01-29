# ------------------------------------- Loading Packages and Data ------------------------------------------

# Loading Libraries
library(lubridate)
library(janitor)
library(zoo)
library(tidyverse)

# Defining Links
pd_sizes_link = "data/overview_data/pe_1960_2022.csv"
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
  mutate(agency_type = case_when(
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

# Filtering data for just 2021
la_pd_sizes_2022 <- la_pd_sizes %>%
  filter(data_year == "2022")

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
average_agency_map <- la_pd_sizes_2022 %>% 
  separate_rows(county_name, sep = ", ") %>%
  group_by(county_name) %>%
  summarize(pct_per_county = mean(total_pe_ct)) %>%
  mutate(county_name = county_name %>% str_to_title())

# Increase in officers per law enforcement agency
average_increase <- la_pd_sizes %>%
  group_by(data_year) %>%
  filter(data_year %in% c("1960", "2022")) %>%
  summarize(ave_officers = mean(total_pe_ct))

# Plotting the average number of offers per 100,000 residents 
officers_per_residents <- la_pd_sizes %>%
  group_by(data_year) %>%
  summarize(ave_per_hundredthousand = 100 * mean(pe_ct_per_1000))

# Number of agencies in 2022
n_agencies_2022 = length(unique(la_pd_sizes_2022$agency_name))

# Number of officers in 2022
n_officers_2022 = sum(la_pd_sizes_2022$total_pe_ct)

# Number of agencies throughout time
n_agencies = length(unique(la_pd_sizes$agency_name))

# Number of police departments
n_pd <- agency_distribution %>%
  filter(agency_type == "Police Department") %>%
  pull(n)
