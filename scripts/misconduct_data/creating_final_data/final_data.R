# Loading Libraries
library(tidyverse)
library(janitor)
library(reticulate)


joblib <- import("joblib")
action_model <- joblib$load(here::here("scripts/misconduct_data/classification_models/finished_models/action_best_model.joblib"))
allegation_model <- joblib$load(here::here("scripts/misconduct_data/classification_models/finished_models/allegation_best_model.joblib"))
disposition_model <- joblib$load(here::here("scripts/misconduct_data/classification_models/finished_models/disposition_best_model.joblib"))

training_allegation <- read_csv("scripts/misconduct_data/creating_training_data/misconduct_allegation.csv")
training_disposition <- read_csv("scripts/misconduct_data/creating_training_data/misconduct_disposition.csv")
training_action <- read_csv("scripts/misconduct_data/creating_training_data/misconduct_action.csv")

allegation_lookup_table <- training_allegation %>%
  select(allegation_better, allegation_desc, allegation_classified) %>%
  distinct(allegation_better, allegation_desc, .keep_all = TRUE)

allegation_lookup_table <- training_allegation %>%
  select(agency_name, allegation, allegation_better, allegation_desc, allegation_classified, allegation) %>%
  distinct(allegation_better, allegation_desc, .keep_all = TRUE)

write.csv(allegation_lookup_table, "the1.csv")

action_lookup_table <- training_action %>%
  mutate(action_classified = ifelse(is.na(action_better), 
                                    "No Repercussion Reported",
                                    action_classified)) %>%
  select(action_better, action_classified) %>%
  distinct(action_better, .keep_all = TRUE)

disposition_lookup_table <- training_disposition %>%
  select(disposition, disposition_classified) %>%
  distinct(disposition, .keep_all = TRUE)


# Defining data
allegation_link = "data/misconduct_data/new_data/data_allegation.csv"
personnel_link = "data/misconduct_data/new_data/data_personnel.csv"
agency_locations_link = "data/misconduct_data/new_data/data_agency-reference-list.csv"
history_ids_link = "data/misconduct_data/new_data/data_post-officer-history.csv"

# Loading data
allegations <- read_csv(here::here(allegation_link))
personnel <- read_csv(here::here(personnel_link))
agency_locations <- read_csv(here::here(agency_locations_link))
history_ids <- read_csv(here::here(history_ids_link))

# Renaming "agency_slug" as "agency"
agency_locations <- agency_locations %>%
  rename(agency = agency_slug)

# Creating the misconduct training dataframe
misconduct <- personnel %>%
  
  # Uniting names into a fullname
  unite(col = 'full_name', 
        c('first_name', "middle_name", 'last_name'), 
        sep = " ", 
        na.rm = TRUE, 
        remove = FALSE) %>%
  
  # Joining personnel with historical IDs
  left_join(history_ids %>% 
              select("uid", "history_id"), 
            by = "uid") %>%
  
  # Joining personnel with their allegations
  right_join(allegations, 
             by = c("uid")) %>%
  select(-ends_with(".y")) %>%
  rename("sex" = sex.x,
         "race" = race.x,
         "agency" = agency.x) %>%
  
  # Joining personnel and allegations with the agency locations
  left_join(agency_locations, 
            by = "agency") %>%
  
  mutate(
    action = tolower(action),
    disposition = tolower(disposition),
    allegation = tolower(allegation),
    agency_type = case_when(
      str_detect(tolower(agency_name), "university|college|campus") ~ "University or Campus Police",
      str_detect(tolower(agency_name), "marshal") ~ "Marshal's Office",
      str_detect(tolower(agency_name), "constable") ~ "Constable's Office",
      str_detect(tolower(agency_name), "sheriff") ~ "Sheriff's Office",
      str_detect(tolower(agency_name), "department|police department") ~ "Police Department",
      TRUE ~ "Other Law Enforcement Agency"
    ),
    agency_name = case_when(
      agency_name == "New Orleans Parish Sheriff's Office" ~ "Orleans Parish Sheriff's Office",
      agency_name == "Orleans Constable" ~ "Orleans Parish Constable",
      agency_name == "Jefferson Constable" ~ "Jefferson Parish Constable",
      TRUE ~ agency_name
    ),
    allegation_better = case_when(
      agency_name == "St. Tammany Parish Sheriff's Office" ~ ifelse(str_detect(sub('.*\\-', '', allegation), 
                                                                               "terminated|warning|hours|warmong|suspension|discharged"),
                                                                    trimws(sub("-[^-]*$", "", allegation)),
                                                                    allegation),
      agency_name == "Rayne Police Department" ~ sub(".*(?:officer-|officer;)", "", allegation),
      agency_name == "New Orleans Police Department" ~ trimws(sub(".*[-;:]", "", allegation)),
      agency_name == "Hammond Police Department" ~ sub(".*(?:officer-)", "", allegation),
      agency_name == "Scott Police Department" ~ ifelse(str_detect(allegation, "\\(.*\\)"), 
                                                        str_extract(allegation, "\\((.*?)\\)"),
                                                        allegation),
      agency_name == "Mandeville Police Department" ~ sub(".*(?:conduct unbecoming/|unbecoming behavior/)", "", allegation),
      agency_name == "Terrebonne Parish Sheriff's Office" ~ sub(".*(?::)", "", allegation),
      TRUE ~ allegation
    ),
    uid = ifelse(!is.na(history_id), history_id, uid),
    index = row_number())



misconduct_allegation_final <- misconduct %>% 
  mutate(
    allegation_better = strsplit(as.character(allegation_better), "\\sand\\s|,|/|;|dr.03|dr. 03|dr.  03|sop.4")
    ) %>%
  unnest(allegation_better) %>%
  filter(!is.na(allegation)) %>%
  left_join(allegation_lookup_table, by = c("allegation_better", "allegation_desc")) %>%
  mutate(allegation_classified = ifelse(is.na(allegation_classified),
                                        allegation_model$predict(.),
                                        allegation_classified)) %>%
  mutate(
    allegation_classified_simplified = case_when(
      allegation_classified %in% c("Violating Rules",
                                   "General Unbecoming Conduct",
                                   "Truthfulness", 
                                   "Supervision", 
                                   "Prisoner Escape", 
                                   "Policies and Procedures",
                                   "Misuse of Equipment and Property",
                                   "Mishandling Assets",
                                   "Interfering with Investigations",
                                   "Improper Search, Seizure, Arrest, or Detainment",
                                   "Improper Relationships",
                                   "General Professionalism",
                                   "General Performance of Duty",
                                   "Following Instructions",
                                   "Dissemination of Information",
                                   "Courtesy",
                                   "Cooperation",
                                   "Confidentiality",
                                   "Cleanliness",
                                   "Acting Impartially",
                                   "Neglecting Duty",
                                   "Effectiveness",
                                   "Completing Reports and Forms",
                                   "Policies and Procedures") ~ "Professionalism",
      allegation_classified %in% c("Sexual Misconduct", 
                                   "Domestic Violence") ~ "Gender-based Offenses",
      allegation_classified %in% c("Use of Force", 
                                   "Harassment and Intimidation") ~ "Force and Harassment",
      allegation_classified %in% c("Miscellanious Allegation",
                                   "Discrimination", 
                                   "Adherence to Law",
                                   "Use of Substances",
                                   "Theft",
                                   "Abuse of Position") ~ "Other Allegation",
      str_detect(allegation_classified, "; ") ~ "Multiple Allegations",
      TRUE ~ allegation_classified
    )
  )

misconduct_disposition_final <- misconduct %>%
  left_join(disposition_lookup_table, by = "disposition") %>%
  filter(!is.na(disposition)) %>%
  mutate(disposition_classified = ifelse(is.na(disposition_classified),
                                         disposition_model$predict(.),
                                         disposition_classified)) %>%
  mutate(
    disposition_classified_simplified = case_when(
      disposition_classified %in% c("Sustained", "Founded", "Partially Sustained") ~ "Sustained",
      disposition_classified %in% c("No Investigation Merited", "Unfounded", "Not Sustained", "Exonerated", "Withdrawn", "Cancelled", "Justified") ~ "Unsustained",
      disposition_classified %in% c("Hearing", "Invalid Complaint", "Miscellanious Disposition", "No Disposition Reported", "Pending Investigation", "Admin Review", "Inconclusive", "Negotiated Settlement", "DI-2", "Active Investigation", "Duplicate Allegation", "Investigation Terminated", "Administratively Closed", "Referred", "Resigned") ~ "Other",
      disposition_classified %in% c("Conviction", "Suspended", "Terminated", "Reprimand") ~ "Repercussion"    )
  )


misconduct_action_final <- misconduct %>%
  mutate(action_better = strsplit(as.character(action), "/|;| and |\\|")) %>%
  unnest(action_better) %>%
  left_join(action_lookup_table, by = "action_better") %>%
  filter(!is.na(action_better)) %>%
  mutate(action_classified = ifelse(is.na(action_classified),
                                    action_model$predict(.),
                                    action_classified)) %>%
  mutate(
    action_classified_simplified = case_when(
      action_classified %in% c("Warning", "Suspension", "Terminated", "Counseling", "Reprimand", "Loss of Privileges", "Training", "Demoted", "Investigation", "Transferred", "Reduced Pay", "Evaluation") ~ "Consequence",
      action_classified %in% c("Decertified", "Arrested") ~ "Outside",
      action_classified %in% c("Resigned", "No Repercussion", "No Consequence Necessary") ~ "No Consequence",
      action_classified %in% c("Miscellanious Repercussion", "Pending", "DM-1", "Referred") ~ "Other",
    )
  )



it <- misconduct_allegation_final %>%
  tabyl(allegation_classified, agency_name) %>%
  adorn_totals(where = "col") %>% 

write.csv(it, "it.csv")
