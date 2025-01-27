# ------------------- Importing Libraries and Cleaning Data -----------

# Loading Libraries
library(tidyverse)
library(janitor)
library(stringdist)
library(tigris)
library(gsheet)
library(googlesheets4)

rename_column_n_to_count <- function(data_frames) {
  result <- lapply(data_frames, function(df) {
    if ("n" %in% colnames(df)) {
      colnames(df)[colnames(df) == "n"] <- "Count"
    }
    colnames(df) <- str_to_title(colnames(df))
    colnames(df) <- gsub("_clean", "", colnames(df))
    
    if ("Percent" %in% colnames(df)) {
      df <- df[, !colnames(df) %in% "Percent"]
    }
    
    if ("Valid_percent" %in% colnames(df)) {
      df <- df[, !colnames(df) %in% "Valid_percent"]
    }
    
    return(df)
  })
  
  return(result)
}

# Loading spreadsheet
spreadsheet_link <- "https://docs.google.com/spreadsheets/d/1jfkRvr2wW3NXbLCe9FLXVX0TKZIgwXmI1r-d8aQ59Eo/edit?gid=0#gid=0"
gs4_auth(email = "eappelson@laaclu.org")

# Loading in city names 
cities_louisiana <- places(state = "LA") %>%
  pull(NAME)

# Loading county names
parishes_louisiana <- counties(state = "LA") %>%
  pull(NAME)

# Defining the most data's date
data_date <- "2025-01-27"

# Importing data 
df <- read_csv(here::here(paste0("data/intake_data/", data_date, "/louisiana_police_misconduct_data_collection.csv"))) %>%
  clean_names()

# Renaming variables
names(df) <- substr(names(df), 1, 20)

df_n <- df %>%
  filter(last_name != "Appelson") %>%
  filter(!(first_name == last_name))

# Cleaning our dataframe
df_clean <- df %>%
  
  # Filtering to only authorized responses
  filter(i_authorize_the_aclu == "X") %>%
  mutate(
    
    # Turning incidents into their categories
    location_of_incident = case_when(
      location_of_incident == 1 ~ "School or University",
      location_of_incident == 2 ~ "Public Street",
      location_of_incident == 3 ~ "Public Transit",
      location_of_incident == 4 ~ "Place of Worship",
      location_of_incident == 5 ~ "Public Park",
      location_of_incident == 6 ~ "Private Residence",
      location_of_incident == 7 ~ "Store",
      location_of_incident == 8 ~ "Protest",
      location_of_incident == 9 ~ "Traffic",
      location_of_incident == 10 ~ "Jail",
      location_of_incident == 11 ~ "Prison",
      TRUE ~ location_of_incident
    ),
    
    # Cleaning parish names
    cleaned_parish = gsub(" parish", "", tolower(parish_of_incident)),
    
    # Classifying parishes with our parish list with a max distance of 10
    cleaned_parish = parishes_louisiana[amatch(parish_of_incident, parishes_louisiana, maxDist = 10)],
    
    # Classifying cities with our city list with a max distance of 3
    cleaned_city = cities_louisiana[amatch(str_to_title(city_of_incident), cities_louisiana, maxDist = 3)],
    
    # Reformatting dates
    date = as.Date(str_sub(time, 1, 10), "%m/%d/%Y"),
    incident_date = as.Date(date_of_incident, "%m/%d/%y")
  ) %>%
  
  # Removing fake submissions
  filter(last_name != "Appelson") %>%
  filter(!(first_name == last_name)) %>%
  
  # Selecting / Renaming variables
  select(submit_date = date, 
         first_name,
         last_name,
         incident_date,
         parish = cleaned_parish,
         city = cleaned_city,
         location = location_of_incident,
         location_other = other_23,
         police = police_department_s_,
         victim = this_incident_happen,
         victim_other = other_26,
         "Wrongful Stop" = wrongfully_stopped,
         "Wrongful Search" = wrongfully_searched,
         "Wrongful Arrest" = wrongfully_arrested,
         "Excessive Use of Force" = wrongfully_subjected,
         "Wrongful Aquisition of Property" = property_wrongfully_,
         "Police Killing" = a_loved_one_was_kill,
         "Other Misconduct" = other_34,
         charges = is_the_victim_of_thi,
         charges_other = other_36,
         narrative = in_my_words_this_is_,
         dob = the_victims_date_of_,
         race = the_victim_identifie,
         perceived_race = the_victim_is_usuall,
         gender = the_victims_gender_i,
         disability = the_victim_has_a_phy,
         "Race" = race,
         "Gender" = gender,
         "Sexuality" = sexuality,
         "Disability" = disability_or_mental,
         "None" = none,
         "Other targeting" = other_48,
         targeted_other_desc = other_49,
         impact = the_incident_s_negat
  ) %>%
  mutate(
    race_clean = case_when(
      str_detect(tolower(race), "mixed|,") ~ "Mixed Race",
      str_detect(tolower(race),"afric|black|blk|balck") ~ "Black/African American",
      str_detect(tolower(race), "cauc|white|whire") ~ "White",
      str_detect(tolower(race), "asia") ~ "Asian",
      str_detect(tolower(race), "native|indigenous|somoa") ~ "Native American",
      is.na(race) ~ "Unknown race",
      TRUE ~ "Other race"
    ),
    gender_clean = case_when(
      str_detect(tolower(gender), "femal|woman|women") ~ "Female",
      str_detect(tolower(gender), "male|man|men|mail") ~ "Male",
      str_detect(tolower(gender), "non-binary|nonbinary") ~ "Non-binary",
      is.na(gender) ~ "Unknown gender",
      TRUE ~ "Other gender"
    ),
    disability_clean = case_when(
      disability == "FALSE" ~ "Victim does not have a disability",
      disability == "TRUE" ~ "Victim has a disability",
      disability == "Prefer not to answer" ~ "Prefered not to answer",
      is.na(disability) ~ "Unknown disability"
    ),
    parish = replace_na(parish, "Unknown parish"),
    city = replace_na(city, "Unknown city"),
    location = replace_na(location, "Unknown location"),
    location = str_replace(location, "Other:", "Other location"),
    victim = case_when(
      victim == "Me" ~ "Self-reported",
      victim %in% c("A family member", "Someone I know") ~ "Reported on someone's behalf",
      victim == "Other:" ~ "Other reporting"
    ),
    impact = case_when(
      impact == "Greatly" ~ "Greatly negatively impacted well-being",
      impact == "Not at all" ~ "Did not negatively impact well-being",
      impact == "Prefer not to answer" ~ "Prefers not to answer",
      impact == "Somewhat" ~ "Somewhat negatively impacted well-being",
      is.na(impact) ~ "Unknown impact"
    ),
    id = row_number()
  )

df %>%
  filter(sid %in% 
           c("87360301",
             "89583007",
             "89673355",
             "88069441", 
             "89658949", 
             "89670766", 
             "89660077", 
             "89673985", 
             "89736235", 
             "87360301", 
             "91986436",
             "89358424", 
             "89659090",
             "89659084", 
             "89658982",
             "89659039", 
             "89736235", 
             "89057746", 
             "89659186")) %>%
  write_csv("immigration.csv")

# ----------------------- Data Analysis -----------------------------------

# Submissions per month
sub_per_month <- df_clean %>% 
  mutate(month = format(submit_date, "%Y-%m")) %>%  # Extract month from the submission date
  tabyl(month) %>%  # Tabulate counts by month
  print()

# Tabulate counts by parish
parish_summary <- df_clean %>% 
  tabyl(parish) %>% 
  arrange(-n) %>% 
  print()

# Tabulate counts by city
city_summary <- df_clean %>% 
  tabyl(city) %>% 
  arrange(-n) %>% 
  print()

# Tabulate counts by location type
location_type_summary <- df_clean %>% 
  tabyl(location) %>% 
  arrange(-n) %>% 
  print()

# Police involvement analysis
police_involved_summary <- df_clean %>% 
  mutate(police = strsplit(tolower(police), ",| and | or|;")) %>%  # Split police data into multiple entries
  unnest(police) %>% 
  mutate(police = case_when(
    police == "jpso" ~ "jefferson parish",  # Standardize specific values
    TRUE ~ police
  )) %>% 
  tabyl(police) %>% 
  arrange(police) %>% 
  print()

# Tabulate counts by victim type
victim_summary <- df_clean %>% 
  tabyl(victim) %>% 
  arrange(-n) %>% 
  print()

# Violence type analysis
violence_type_summary <- df_clean %>% 
  mutate(across(c("Wrongful Stop":"Other Misconduct"), ~ ifelse(!is.na(.), cur_column(), NA))) %>% 
  pivot_longer(
    cols = c("Wrongful Stop":"Other Misconduct"),
    names_to = "category",
    values_drop_na = TRUE
  ) %>% 
  tabyl(category) %>% 
  arrange(-n) %>% 
  print()

# Tabulate counts by race
race_summary <- df_clean %>% 
  tabyl(race) %>% 
  arrange(-n) %>% 
  print()

# Tabulate counts by cleaned race
race_cleaned_summary <- df_clean %>% 
  tabyl(race_clean) %>% 
  arrange(-n) %>% 
  print()

# Tabulate counts by gender
gender_summary <- df_clean %>% 
  tabyl(gender) %>% 
  arrange(-n) %>% 
  print()

# Tabulate counts by cleaned gender
gender_cleaned_summary <- df_clean %>% 
  tabyl(gender_clean) %>% 
  arrange(-n) %>% 
  print()

# Tabulate counts by disability
disability_summary <- df_clean %>% 
  tabyl(disability_clean) %>% 
  arrange(-n) %>% 
  print()

# Targeted reasons analysis
target_reason_summary <- df_clean %>% 
  mutate(across(c("Race":"Other targeting"), ~ ifelse(. == "X", cur_column(), NA))) %>% 
  pivot_longer(
    cols = c("Race":"Other targeting"),
    names_to = "category",
    values_drop_na = TRUE
  ) %>% 
  tabyl(category) %>% 
  arrange(-n) %>% 
  print()

# Tabulate counts by impact
impact_summary <- df_clean %>% 
  tabyl(impact) %>% 
  print()

# ----------------------- Bivariate Analysis -----------------------------
# Cross-tabulate location and race
location_race <- df_clean %>% 
  tabyl(location, race_clean) %>% 
  print()

# Cross-tabulate location and gender
location_gender <- df_clean %>% 
  tabyl(location, gender_clean) %>% 
  print()

# Cross-tabulate location and disability
location_disability <- df_clean %>% 
  tabyl(location, disability_clean) %>% 
  print()

# Cross-tabulate location and violence type
location_violence <- df_clean %>% 
  mutate(across(c("Wrongful Stop":"Other Misconduct"), ~ ifelse(!is.na(.), cur_column(), NA))) %>% 
  pivot_longer(
    cols = c("Wrongful Stop":"Other Misconduct"),
    names_to = "category",
    values_drop_na = TRUE
  ) %>% 
  tabyl(location, category) %>% 
  print()

# Cross-tabulate location and target reasons
location_target <- df_clean %>% 
  mutate(across(c("Race":"Other targeting"), ~ ifelse(. == "X", cur_column(), NA))) %>% 
  pivot_longer(
    cols = c("Race":"Other targeting"),
    names_to = "category",
    values_drop_na = TRUE
  ) %>% 
  tabyl(location, category) %>% 
  print()

# Cross-tabulate violence type and race
violence_race <- df_clean %>% 
  mutate(across(c("Wrongful Stop":"Other Misconduct"), ~ ifelse(!is.na(.), cur_column(), NA))) %>% 
  pivot_longer(
    cols = c("Wrongful Stop":"Other Misconduct"),
    names_to = "category",
    values_drop_na = TRUE
  ) %>% 
  tabyl(race_clean, category) %>% 
  print()

# Cross-tabulate violence type and gender
violence_gender <- df_clean %>% 
  mutate(across(c("Wrongful Stop":"Other Misconduct"), ~ ifelse(!is.na(.), cur_column(), NA))) %>% 
  pivot_longer(
    cols = c("Wrongful Stop":"Other Misconduct"),
    names_to = "category",
    values_drop_na = TRUE
  ) %>% 
  tabyl(gender_clean, category) %>% 
  print()

# Cross-tabulate violence type and disability
violence_disability <- df_clean %>% 
  mutate(across(c("Wrongful Stop":"Other Misconduct"), ~ ifelse(!is.na(.), cur_column(), NA))) %>% 
  pivot_longer(
    cols = c("Wrongful Stop":"Other Misconduct"),
    names_to = "category",
    values_drop_na = TRUE
  ) %>% 
  tabyl(disability_clean, category) %>% 
  print()

# Cross-tabulate target reasons and race
target_race <- df_clean %>% 
  mutate(across(c("Race":"Other targeting"), ~ ifelse(. == "X", cur_column(), NA))) %>% 
  pivot_longer(
    cols = c("Race":"Other targeting"),
    names_to = "category",
    values_drop_na = TRUE
  ) %>% 
  tabyl(race_clean, category) %>% 
  print()

# Cross-tabulate target reasons and gender
target_gender <- df_clean %>% 
  mutate(across(c("Race":"Other targeting"), ~ ifelse(. == "X", cur_column(), NA))) %>% 
  pivot_longer(
    cols = c("Race":"Other targeting"),
    names_to = "category",
    values_drop_na = TRUE
  ) %>% 
  tabyl(gender_clean, category) %>% 
  print()

# Cross-tabulate target reasons and disability
target_disability <- df_clean %>% 
  mutate(across(c("Race":"Other targeting"), ~ ifelse(. == "X", cur_column(), NA))) %>% 
  pivot_longer(
    cols = c("Race":"Other targeting"),
    names_to = "category",
    values_drop_na = TRUE
  ) %>% 
  tabyl(disability_clean, category) %>% 
  print()

# Cross-tabulate race and gender
race_gender <- df_clean %>% 
  tabyl(race_clean, gender_clean) %>% 
  print()

# Cross-tabulate race and disability
race_disability <- df_clean %>% 
  tabyl(race_clean, disability_clean) %>% 
  print()

# Cross-tabulate gender and disability
gender_disability <- df_clean %>% 
  tabyl(gender_clean, disability_clean) %>% 
  print()



### ------------ Statistics ---------

# Number of submissions
n_submission <- nrow(df_clean) %>% 
  print()

# Number of greatly negatively impacted
n_great_neg_impact <- df_clean %>%
  filter(impact == "Greatly negatively impacted well-being") %>%
  nrow() %>% 
  print()

# Number of parishes
n_parish <- df_clean %>%
  tabyl(parish) %>%
  filter(!(parish %in% c("Unknown parish", "Other parish"))) %>%
  nrow() %>% 
  print()

# Number of excessive force and police killings
n_violent <- (sum(df_clean$`Police Killing` == "X", na.rm = TRUE) + 
                sum(df_clean$`Excessive Use of Force` == "X", na.rm = TRUE)) %>% 
  print()

# Percent non-white 
perc_non_white <- df_clean %>%
  filter(race_clean != "Unknown race") %>%
  tabyl(race_clean) %>%
  mutate(percent = n / sum(n)) %>%
  filter(race_clean != "White") %>%
  summarise(percent_non_white = sum(percent)) %>%
  pull(percent_non_white) %>% 
  print()

# Percent disability
percent_disbility <- df_clean %>%
  filter(disability_clean != "Unknown disability") %>%
  tabyl(disability_clean) %>%
  mutate(percent = n / sum(n)) %>%
  filter(disability_clean == "Victim has a disability") %>%
  summarise(percent_dis = sum(percent)) %>%
  pull(percent_dis) %>% 
  print()

# Highest reporting
high_reporting <- df_clean %>%
  tabyl(parish) %>%
  filter(parish != "Unknown parish") %>%
  arrange(-n) %>%
  head(3) %>%
  pull(parish) %>%
  paste(., collapse = ", ")






# ------------------------- Exporting content into a Google Sheet --------------------------------------

# Defining sheets for the spreadsheet
sheets = c("Report per Month", "Report per Parish", "Report per City", "Report per Location", 
           "Report per Department", "Report per Submiter", "Report per Misconduct", 
           "Report per Race (bad)", "Report per Race", "Report per Gender (bad)", 
           "Report per Gender", "Report per Disability", "Report per Target Reason", 
           "Report per Impact", "Race by Location", "Gender by Location", "Disability by Location", 
           "Misconduct by Location", "Target by Location", "Misconduct by Race", "Misconduct by Gender", 
           "Misconduct by Disability", "Target by Race", "Target by Gender", "Target by Disability", 
           "Race and Gender", "Race and Disability", "Gender and Disability")


# Defining sheet values
data_frames = list(sub_per_month, 
                   parish_summary, 
                   city_summary, 
                   location_type_summary, 
                   police_involved_summary, 
                   victim_summary, 
                   violence_type_summary, 
                   race_summary, 
                   race_cleaned_summary, 
                   gender_summary, 
                   gender_cleaned_summary, 
                   disability_summary, 
                   target_reason_summary, 
                   impact_summary, 
                   location_race, 
                   location_gender, 
                   location_disability, 
                   location_violence, 
                   location_target, 
                   violence_race, 
                   violence_gender, 
                   violence_disability, 
                   target_race, 
                   target_gender, 
                   target_disability, 
                   race_gender, 
                   race_disability, 
                   gender_disability)


modified_data_frames <- rename_column_n_to_count(data_frames)

for (i in seq_along(sheets)) {
  sheet_name <- sheets[i]
  data_frame <- modified_data_frames[[i]]
  
  write_sheet(data_frame, ss = spreadsheet_link, sheet = sheet_name)
}
